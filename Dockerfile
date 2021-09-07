ARG OUTPUT_DIR=/root/output
ARG EXECUTABLE_NAME=bootstrap

FROM haskell-base:1.0 as build

# Build the lambda
RUN mkdir /opt/haskell-lambda && cd /opt/haskell-lambda
WORKDIR /opt/haskell-lambda
COPY . /opt/haskell-lambda

RUN stack clean --full
RUN stack build

ARG OUTPUT_DIR
ARG EXECUTABLE_NAME

RUN mkdir ${OUTPUT_DIR}

RUN cp $(stack path --local-install-root)/bin/${EXECUTABLE_NAME} ${OUTPUT_DIR}/${EXECUTABLE_NAME}

ENTRYPOINT sh


#build deployable docker image
FROM public.ecr.aws/lambda/provided:al2 as deploy
RUN yum install -y postgresql-devel
ARG EXECUTABLE_NAME
ARG OUTPUT_DIR

WORKDIR ${LAMBDA_RUNTIME_DIR}
COPY --from=build ${OUTPUT_DIR} .

#RUN ls
#RUN mv ${EXECUTABLE_NAME} bootstrap || true
RUN ls

CMD [ "handler" ]
# docker haskell lambda
# docker run -p 9000:8080 random-letter:latest
# curl -XPOST "http://localhost:9000/2015-03-31/functions/function/invocations" -d '{"personName": "Jorge", "personAge": 30}'
# curl -XPOST "http://192.168.99.100:9000/2015-03-31/functions/function/invocations" -d '{"personName": "Jorge", "personAge": 30}'