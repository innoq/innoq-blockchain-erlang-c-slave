FROM erlang:20

COPY . /usr/src
WORKDIR /usr/src

RUN rebar3 clean && rebar3 compile
RUN mv priv/src.so priv/innoq-blockchain-erlang-c-slave.so

ENTRYPOINT rebar3 shell --apps slave --name ${HOSTNAME}@${OWN_IP} --setcookie secret
