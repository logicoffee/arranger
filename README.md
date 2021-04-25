# Arranger

ダーツのアレンジを返す API
LINE Bot 用に作成

```
docker run --rm -e CHANNEL_TOKEN=foo -e CHANNEL_SECRET=bar arranger
```

## Secret 登録

```
gcloud --project darts-line-bots secrets create arranger --data-file .tmp/secrets.txt
```
