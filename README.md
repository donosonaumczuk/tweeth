# tweeth

Twitter bot that connects to Infura websockets to listen some Ethereum Smart Contracts events and tweet them.

Built in Haskell for Functional Programming class @ITBA.

## Build

```
stack build
```

## Run

### Setting the Infura Project ID
In order to avoid exposing the Infura Project ID (kind of API Key), you must set it as `TWEETH_INFURA_PROJECT_ID` environment variable.
Supposing your Infura Proyect ID is `myT35t1nFu84pr0y3ct1d` you can do it with the following command:
 
```
export TWEETH_INFURA_PROJECT_ID=myT35t1nFu84pr0y3ct1d
```

### Execute run command
```
stack exec tweeth-exe
```
