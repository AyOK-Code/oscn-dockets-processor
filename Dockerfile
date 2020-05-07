FROM asemio/mountain-caravan:0.2.2 AS build
WORKDIR /app
RUN sudo apk update \
  && sudo apk add --no-cache perl cmake ca-certificates

COPY oscn-dockets-processor.opam .

RUN OPAMYES=1 opam install . --deps-only \
  && OPAMYES=1 opam reinstall ssl

COPY . .

RUN sudo chown -R opam /app \
  && opam exec -- dune build src/cli/app.exe \
  && cp /app/_build/default/src/cli/app.exe . \
  && chmod 755 app.exe \
  && strip app.exe

RUN mkdir lib \
  && ldd app.exe >> deps.txt \
  && cat deps.txt | awk '$2 == "=>" && $3 !~ /ld-musl/ {print $1, $3}' | sort | uniq | awk '{print $2}' | xargs -n1 -I{} -- cp {} lib/


##############################
FROM alpine:3.11
WORKDIR /app
COPY --from=build /app/app.exe .
COPY --from=build /app/lib ./lib/
RUN apk add --no-cache ca-certificates
ENV LD_LIBRARY_PATH ./lib
RUN ./app.exe --help

ENTRYPOINT [ "/app/app.exe", "serve" ]
