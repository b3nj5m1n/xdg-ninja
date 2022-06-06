FROM fpco/stack-build:lts-19.10

WORKDIR /usr/lib/gcc/x86_64-linux-gnu/7.5.0
RUN ls
RUN cp crtbeginT.o crtbeginT.o.orig
RUN cp crtbeginS.o crtbeginT.o

RUN git clone "https://github.com/b3nj5m1n/xdg-ninja" /tmp/xdg-ninja
WORKDIR /tmp/xdg-ninja
RUN stack install --ghc-options '-optl-static -fPIC'
