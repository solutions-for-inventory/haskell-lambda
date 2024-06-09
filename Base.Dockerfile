FROM public.ecr.aws/amazonlinux/amazonlinux:2023 as build

# Build the lambda
RUN mkdir /opt/haskell-lambda && cd /opt/haskell-lambda
WORKDIR /opt/haskell-lambda
COPY . /opt/haskell-lambda

RUN yum install -y tar xz git gcc-c++ make libffi zlib-devel gmp-devel postgresql-devel
#RUN yum install -y tar xz git gcc make libffi zlib gmp-devel glibc-static gmp-static  shadow-utils zlib-devel postgresql-devel xz
RUN curl -sSL https://get.haskellstack.org/ | sh
RUN stack setup
RUN stack clean --full