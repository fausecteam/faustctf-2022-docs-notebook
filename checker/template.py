#!/usr/bin/env python3

from ctf_gameserver import checkerlib

import requests

import logging
import utils
from user_agents import USER_AGENTS
import random
import secrets
import time
import tempfile
import difflib

import subprocess

import os
dirname = os.path.dirname(__file__)
exporter = os.path.join(dirname, '../docsnotebook/src/export.el')

# Urllib is complaining about unverified HTTPS Requests
# I know, shut up...
import urllib3

urllib3.disable_warnings()

class DocsNotebookChecker(checkerlib.BaseChecker):

    def url(self, path):
        return f'https://[{self.ip}]:9000{path}'

    def register(self, S):
        username = secrets.token_hex(secrets.randbelow(11) + 10)
        password = secrets.token_hex(secrets.randbelow(11) + 10)

        logging.info(f'Registering user {username} with password {password}')
        r = S.post(self.url('/register'), data={'username': username, 'password': password})
        if r.status_code != 200:
            logging.info(f'Account registration failed with code {r.status_code}: {r.content}')
            return (None, None)

        return (username, password)

    def place_flag(self, tick):
        S = requests.Session()
        S.header = {"User-Agent": random.choice(USER_AGENTS)}
        S.verify = False
        username, password = self.register(S)
        if not username:
            return checkerlib.CheckResult.FAULTY

        flag = checkerlib.get_flag(tick)
        logging.info(f'Placing flag {flag}')
        r = S.post(self.url('/edit/flag'), data=flag)
        if r.status_code != 200:
            logging.info(f'Placing flag failed with code {r.status_code}: {r.content}')
            return checkerlib.CheckResult.FAULTY

        checkerlib.store_state(str(tick), (username, password))
        checkerlib.set_flagid(f'{username}/flag.org')

        return checkerlib.CheckResult.OK

    def check_service(self):
        logging.info('Check service')
        S = requests.Session()
        S.header = {"User-Agent": random.choice(USER_AGENTS)}
        S.verify = False
        username, password = self.register(S)
        if not username:
            return checkerlib.CheckResult.FAULTY

        files = utils.generate_org_files()
        random.shuffle(files)

        with tempfile.TemporaryDirectory() as export:
            for orgfile in files[:random.randint(1, 5)]:
                logging.info('---------------------------------------------------------------------')
                logging.info(f'Starting export:\n{orgfile}')
                path = secrets.token_hex(random.randint(8, 16))
                r = S.post(self.url(f'/edit/{path}'), data=orgfile)
                if r.status_code != 200:
                    logging.info(f'Failed to save as {path}')
                    return checkerlib.CheckResult.FAULTY
                r = S.post(self.url(f'/export/{path}'))
                if r.status_code != 200:
                    logging.info(f'Failed to export path {path}')
                    return checkerlib.CheckResult.FAULTY

                with open(f'{export}/{path}.org', 'w') as f:
                    f.write(orgfile)

                proc = subprocess.run([exporter, f'{export}/{path}.org'], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
                if proc.returncode != 0:
                    logging.info(f'Exporting failed with exit code {proc.returncode}:')
                    logging.info("Stdout:")
                    logging.info(proc.stdout)
                    logging.info("Stderr:")
                    logging.info(proc.stderr)
                    # Easiest way to raise an exception ;)
                    # I don't know whether it's possible to create a "not checked" manually
                    proc.check_returncode()

                time.sleep(5)

                with open(f'{export}/{path}.html', 'rb') as f:
                    localdata = f.read()

                remotedata = S.get(self.url(f'/export/{path}')).content

                if localdata != remotedata:
                    logging.info(f'Exported html does not match: length {len(localdata)} (local) vs {len(remotedata)} (remote)')
                    logging.info("Diff (if valid utf-8):")
                    try:
                        for line in difflib.unified_diff(localdata.decode().split('\n'), remotedata.decode().split('\n'), fromfile='local', tofile='remote', lineterm=''):
                            print(line)
                    except:
                        pass
                    return checkerlib.CheckResult.FAULTY

        return checkerlib.CheckResult.OK

    def check_flag(self, tick):
        state = checkerlib.load_state(str(tick))
        if not state:
            logging.info(f'No state information found for tick {tick}')
            return checkerlib.CheckResult.FLAG_NOT_FOUND
        username, password = state

        S = requests.Session()
        S.header = {"User-Agent": random.choice(USER_AGENTS)}
        S.verify = False
        logging.info(f'Logging in with user {username} and password {password}')
        r = S.post(self.url('/login'), data={'username': username, 'password': password})
        if r.status_code != 200:
            logging.info(f'Login failed with code {r.status_code}: {r.content}')
            return checkerlib.CheckResult.FAULTY

        r = S.get(self.url('/edit/flag'))
        if r.status_code != 200:
            # Reading a nonexistent file returns code 200 with empty content
            # Therefore another code indicates a server error (and not a missing flag)
            logging.info(f'Reading file failed with code {r.status_code}: {r.content}')
            return checkerlib.CheckResult.FAULTY

        flag = checkerlib.get_flag(tick).encode()
        if r.content != flag:
            logging.info(f'Flag mismatch "{r.content}" != "{flag}"')
            return checkerlib.CheckResult.FLAG_NOT_FOUND

        return checkerlib.CheckResult.OK


if __name__ == '__main__':

    checkerlib.run_check(DocsNotebookChecker)
