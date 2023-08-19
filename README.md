# News API

This haskell server can be used to fetch a news article searchable by title, author, keywords in description or list n relevant news articles,
using https://gnews.io apis

## Features
- search api
```
curl --location --request GET 'http://localhost:8081/news/search?keyword=ok&title=world&author=Ok'

{
    "articles": [
        {
            "title": "Sample Title",
            "description": "Hello World,
            "url": "<URL>",
            "image": "<URL>",
            "source": {
                "name": "XYZ",
                "url": "<URL>"
            }
        }
    ]
}
```

- list api
```
curl --location --request GET 'http://localhost:8081/news/top?count=3'

{
    "articles": [
        {
            "title": "Sample Title",
            "description": "Hello World,
            "url": "<URL>",
            "image": "<URL>",
            "source": {
                "name": "XYZ",
                "url": "<URL>"
            }
        },
        {
            "title": "Sample Title 2",
            "description": "Hello World,
            "url": "<URL>",
            "image": "<URL>",
            "source": {
                "name": "XYZ",
                "url": "<URL>"
            }
        }
        ,{
            "title": "Sample Title 3",
            "description": "Hello World,
            "url": "<URL>",
            "image": "<URL>",
            "source": {
                "name": "XYZ",
                "url": "<URL>"
            }
        }
    ]
}
```

## Installation
- install ghc using [link](https://cabal.readthedocs.io/en/3.4/getting-started.html)
- cabal build
- cabal run
- You can export your own api key for interacting with gnews by exporting env SERVICE_API_KEY , default is provided in code