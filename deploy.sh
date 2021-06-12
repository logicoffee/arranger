#!/bin/bash
set -eu

image_name=asia.gcr.io/darts-line-bots/arranger:$2

case $1 in
    "build" ) docker build -t $image_name --build-arg EXECUTABLE=arranger . ;;
    "push" ) docker push $image_name ;;
    "deploy" )
        secrets=($(gcloud --project darts-line-bots secrets versions access latest --secret arranger))
        gcloud --project darts-line-bots run deploy arranger --image $image_name --set-env-vars $(IFS=","; echo "${secrets[*]}") --platform managed --region asia-northeast1 --port 8000 --cpu 1 --memory 128 --allow-unauthenticated ;;
esac
