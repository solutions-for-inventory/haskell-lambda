# haskell-lambda

`docker run -p 9000:8080 ghcr.io/solutions-for-inventory/hlambda-gql:rel8`\
`curl -XPOST "http://localhost:9000/2015-03-31/functions/function/invocations" -d '{"personName": "Jorge", "personAge": 30}'`\
`curl -XPOST "http://192.168.99.100:9000/2015-03-31/functions/function/invocations" -d '{"personName": "Jorge", "personAge": 30}'`

aws ecr get-login-password --region us-west-2 | docker login --username AWS --password-stdin 794576056465.dkr.ecr.us-west-2.amazonaws.com
docker tag ghcr.io/solutions-for-inventory/hlambda-gql:rel8 794576056465.dkr.ecr.us-west-2.amazonaws.com/janez/hlambda-gql:rel8
docker push 794576056465.dkr.ecr.us-west-2.amazonaws.com/janez/hlambda-gql:rel8
