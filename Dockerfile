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

ARG EXECUTABLE_NAME
ARG OUTPUT_DIR

WORKDIR ${LAMBDA_RUNTIME_DIR}
COPY --from=build ${OUTPUT_DIR} .

#RUN ls
#RUN mv ${EXECUTABLE_NAME} bootstrap || true
RUN ls

CMD [ "handler" ]

#curl -XPOST "http://localhost:9000/2015-03-31/functions/function/invocations" -d '{"personName": "Jorge", "personAge": 30}'