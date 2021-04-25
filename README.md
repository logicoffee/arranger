# Arranger

ダーツのアレンジを返す API
LINE Bot 用に作成

```
docker build -t asia.gcr.io/darts-line-bots/arranger:v1.0.0 --build-arg EXECUTABLE=arranger .
docker tag arranger:v0.1.2 asia.gcr.io/darts-line-bots/arranger:v0.1.2
docker push asia.gcr.io/darts-line-bots/arranger:v1.0.0
```
