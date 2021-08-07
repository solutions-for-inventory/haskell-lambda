ARG OUTPUT_DIR=/root/output
ARG EXECUTABLE_NAME=bootstrap

FROM public.ecr.aws/lambda/provided:al2 as build

SHELL ["/bin/bash", "--rcfile", "~/.profile", "-c"]

# Switching to root user
USER root
RUN mkdir /opt/bin

#RUN yum -y install wget tar gzip xz make automake gcc gmp-devel libffi zlib zlib-devel
# Installing basic dependencies
RUN yum install -y \
    tar gzip\
    xz \
    make automake \
    gcc gmp-devel \
    zlib zlib-devel \
    postgresql-devel \
    libicu libicu-devel \
    libyaml libyaml-devel

# Installing Haskell Stack
RUN curl -L -o stack-2.7.3-linux-x86_64.tar.gz https://github.com/commercialhaskell/stack/releases/download/v2.7.3/stack-2.7.3-linux-x86_64.tar.gz \
    && tar -zxf stack-2.7.3-linux-x86_64.tar.gz \
    && mv stack-2.7.3-linux-x86_64/stack /opt/bin/stack \
    && chmod +x /opt/bin/stack \
    && rm stack-2.7.3-linux-x86_64.tar.gz

# Installing GHC static dependencies
ARG STACK_RESOLVER=lts-18.4
RUN stack setup --resolver=${STACK_RESOLVER}
RUN stack install --resolver=${STACK_RESOLVER} aeson text bytestring unliftio async
RUN stack install --resolver=${STACK_RESOLVER} http-client http-client-tls http-types
RUN stack install --resolver=${STACK_RESOLVER} string-conv safe hsyslog postgresql-simple


# Build the lambda
RUN mkdir /opt/haskell-lambda && cd /opt/haskell-lambda
WORKDIR /opt/haskell-lambda
COPY . /opt/haskell-lambda

#RUN stack clean --full
RUN stack build

ARG OUTPUT_DIR
ARG EXECUTABLE_NAME

RUN mkdir ${OUTPUT_DIR}

RUN cp $(stack path --local-install-root)/bin/${EXECUTABLE_NAME} ${OUTPUT_DIR}/${EXECUTABLE_NAME}

ENTRYPOINT sh


#build deployable docker image
FROM public.ecr.aws/lambda/provided:al2 as deploy

ARG EXECUTABLE_NAME
ARG OUTPUT_DIR

WORKDIR ${LAMBDA_RUNTIME_DIR}
COPY --from=build ${OUTPUT_DIR} .

#RUN ls
#RUN mv ${EXECUTABLE_NAME} bootstrap || true
RUN ls

CMD [ "handler" ]

#curl -XPOST "http://localhost:9000/2015-03-31/functions/function/invocations" -d '{"personName": "Jorge", "personAge": 30}'