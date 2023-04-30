FROM ubuntu:22.04

COPY . /app/

RUN apt-get -yq update && \
    apt-get -y upgrade && \
    apt-get -yq --no-install-suggests --no-install-recommends install \
    ocaml \
    menhir \
    llvm-14 \
    llvm-14-dev \
    m4 \
    clang-14 \
    git \
    libclang-14-dev \
    ca-certificates \
    pkg-config \
    opam \
    valgrind \
    make

RUN ln -s /usr/bin/clang-14 /usr/bin/clang
RUN opam init --disable-sandboxing

WORKDIR /app

RUN make install
RUN opam env >> /root/.bashrc

CMD ["bash"]