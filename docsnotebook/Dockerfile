FROM faust.cs.fau.de:5000/docsnotebook_deps
COPY src /srv
RUN make -C srv && strip srv/export
RUN mkdir -p /srv/files && chmod 777 /srv /srv/files
RUN touch /srv/files/auth.db && chmod 600 /srv/files/auth.db
RUN chown nobody:nogroup /srv/export && chmod +s /srv/export
WORKDIR /srv
ENTRYPOINT ["./server.el"]
EXPOSE 9000/tcp
