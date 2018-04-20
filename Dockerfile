FROM erlang:20

COPY . /usr/src
WORKDIR /usr/src

RUN rebar3 clean && rebar3 compile
RUN mv priv/src.so priv/innoq-blockchain-erlang-c-slave.so

ENV NAME 'sja1@127.0.0.1'

ENTRYPOINT rebar3 shell --apps slave --name ${NAME} --setcookie secret
