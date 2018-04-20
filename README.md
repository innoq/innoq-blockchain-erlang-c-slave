slave
=====

An OTP application

Build
-----

    $ rebar3 compile

Docker
======

    docker build -t blockchain-client .
    docker run -it --rm --name sja1 -e 'NAME=sja1@10.100.110.119' blockchain-client  
