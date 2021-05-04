# tweeth

[Twitter](https://twitter.com/) bot that connects to [Infura](https://infura.io/) websockets to listen to some [Ethereum](https://ethereum.org/) Smart Contract events and tweets them.

Built in [Haskell](https://www.haskell.org/) for Functional Programming class at ITBA.

Read its tweets at [@tweeth_bot](https://twitter.com/tweeth_bot)! ;)

## Setup

### Setting Infura Project ID
In order to avoid exposing the Infura Project ID (kind of API Key), you must set it as `TWEETH_INFURA_PROJECT_ID` environment variable with the following command:
 
```
export TWEETH_INFURA_PROJECT_ID="YOUR-INFURA-PROJECT-ID"
```

### Setting Twitter OAuth

In order to avoid exposing the Twitter OAuth keys, you must set them as four environment variables: `OAUTH_CONSUMER_KEY`, `OAUTH_CONSUMER_SECRET`, `OAUTH_ACCESS_TOKEN` and `OAUTH_ACCESS_SECRET`.

After enabling your Twitter Developer Account, you can find or generate all of them in the [Twitter Developer Portal](https://developer.twitter.com/en/portal/projects-and-apps).
```
export OAUTH_CONSUMER_KEY="YOUR-TWITTER-API-KEY"
```
```
export OAUTH_CONSUMER_SECRET="YOUR-TWITTER-API-SECRET-KEY"
```
```
export OAUTH_ACCESS_TOKEN="YOUR-TWITTER-ACCESS-TOKEN"
```
```
export OAUTH_ACCESS_SECRET="YOUR-TWITTER-SECRET-ACCESS-TOKEN"
```

## Build

```
stack build
```

## Run

```
stack exec tweeth-exe
```

## Tests

```
stack test
```

You can run tests and obtain a coverage report using the following command: 

```
stack test --coverage
```
![image](https://user-images.githubusercontent.com/23125060/116967561-f1f7a300-ac88-11eb-9270-290582287fea.png)

