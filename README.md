# Cardano-server

A lightweight backend server for hosting Cardano dApps. It is an alternative to Plutus Application Backend (PAB).

Key features:

1. Low resource consumption compared to PAB. Fast synchronization with other backend services: Cardano Node, Cardano Wallet Backend, Plutus Chain Index, Kupo.

2. Fetching the actual blockchain data is handled automatically: smart contract developers can focus on the business logic of their apps.

3. Supports our transaction builder (check [this repo](https://github.com/encryptedcoins/plutus-apps-extra)).

4. A console client capable of sending correctly constructed requests to the server is created automatically (or with a few extra lines of code).

5. The client can emulate user behavior by sending randomized requests periodically, which might be helpful for stress-testing your app.

## How to use

### Server

To specialize cardano-server for your own application, first you need to make a type with some information about your API:

```haskell
type ExampleApi = ServerApi 
    ([BuiltinByteString], InputContext) -- A type of request body that we expect to receive in ServerTx and NewTx endpoints
    ExampleApiError                     -- A type of errors that might be thrown while processing user requests to these endpoints
    Bool                                -- A type of request body that we expect to receive in status enpoint
    '[ExampleStatusEndpointError]       -- A list with types of errors that might be thrown in status endpoint
    Text                                -- A type of response in status endpoint
```

If you don't need any additional errors you can use `NoError` and empty list (`'[]`) respectively.
Note that you need to provide `IsCardanoServerError` instance for all new server errors:

```haskell
data ExampleApiError = HasDuplicates
    deriving (Show, Exception)

instance IsCardanoServerError ExampleApiError where
    errStatus _ = toEnum 422
    errMsg _ = "The request contains duplicate tokens and will not be processed."
    
data ExampleStatusEndpointError = ExampleStatusEndpointError
    deriving (Show, Exception)

instance IsCardanoServerError ExampleStatusEndpointError where
    errStatus _ = toEnum 422
    errMsg _ = "This is an example of an error in the status endpoint."
```

Additional to this you need to define an input of your server. It represents the type of external inputs the server may receive during execution.

```haskell
type instance InputOf ExampleApi = [BuiltinByteString]
```

And type with auxiliary environment data that the server must be aware of. Normally, this data contains parameters specific to your application. If you don't need it just use a `()`:

```haskell
type instance AuxillaryEnvOf ExampleApi = ()
```

After that you need to define a handler for `status` endpoint of your server. Note that you should use `toEnvelope` to handle all status endpoint errors.

```haskell
statusEndpointHandler :: Bool -> ServerM ExampleApi (Envelope '[ExampleStatusEndpointError] Text)
statusEndpointHandler b = toEnvelope $ 
    if b 
    then pure "This is an example of a status endpoint." 
    else throwM ExampleStatusEndpointError
```

And lastly, you must define a `ServerHandle`.
In this example request body of tx endpoints is just input with context, but you are free to use any type for this - just provide an appropriate input extraction function.

```haskell    
exampleServerHandle :: ServerHandle ExampleApi
exampleServerHandle = ServerHandle
        Kupo                            -- Default chain index
        ()                              -- Server auxillary env
        ((:[]) <$> getWalletAddr)       {- Actions that return the list of currently tracked Cardano network addresses. 
                                           UTXOs from these addresses can be used for constructing transactions. -}
        (\bbs -> pure [testMintTx bbs]) -- How to build transaction that will handle server input
        (pure ())                       -- Actions that must be performed on repeat whenever the server is idle
        processRequest                  -- How to extract input from request in tx endpoints
        statusEndpointHandler           -- Handler of status endpoint
    where
        processRequest (bbs, ctx) = do
            let hasDuplicates = length bbs /= length (nub bbs)
            when hasDuplicates $ throwM HasDuplicates
            return (sort bbs, ctx)
```

Now you can run your server:

```haskell
runExampleServer :: IO ()
runExampleServer = runServer exampleServerHandle
```

If you need to execute some actions to get the environment, for example if you are reading it from a file, you can do it here:

```haskell
runExampleServer :: IO ()
runExampleServer = do
    myEnv <- someIOComputations
    runServer $ ServerHandle
        Kupo
        myEnv
        ...
```

### Client

The console client allows you to construct and send requests to the server using the command line. Client can work in two different mods. In `auto` mode it will cyclically generate some input and send it to server (you can define frequency of it with command line arguments). In `manual` mode it will receive some text input from command line arguments, build request based on it and send it to server once.</br>
To make client for your cardano server you need to define a `ClientHandle`. It is just a data type with a pair of functions for every server endpoint (to work in manual and auto client mode). Some functions are already predefined (but you still can change them if you want), so you need to provide auto and manual functions only for `newTx`, `serverTx` and `status` endpoints. Or don't provide anything if you don't need this functionality. You can also use already predefined combinators if types of requests of your server have corresponding instances. Alternatively, you can write the whole function by yourself if it contains some complicated logic.

Combinators that you can use:
- `autoWith` - function that takes generator of input and makes auto client based on it.
- `autoWithRandom` - function that makes auto client for requests that have a `Random` instance.
- `manualWith` - function that takes another function that processes text input from command line arguments and builds endpoint input from it.
- `manualWithRead` - function that makes manual client for requests that have a `Read` instance.
- `manualWithJsonFile` - function that receives name of JSON file and parse endpoint input from it. Requires `FromJSON` instance.

Now we can define a client for an example server:

```haskell
runExampleClient :: IO ()
runExampleClient = runClient exampleServerHandle exampleClientHandle

exampleClientHandle :: ClientHandle ExampleApi
exampleClientHandle = def
    { autoNewTx      = autoWith   genInput
    , autoServerTx   = autoWith   genInput
    , autoStatus     = autoWithRandom   
    , manualNewTx    = manualWith readInput
    , manualServerTx = manualWith readInput
    , manualStatus   = manualWithRead
    }

genInput :: ServerM ExampleApi ([BuiltinByteString], InputContext)
genInput = fmap ((,def) . nub) $ liftIO $ do
    inputLength <- randomRIO (1, 15)
    let genBbs = stringToBuiltinByteString <$> (randomRIO (2, 8) >>= (`replicateM` randomIO))
    replicateM inputLength genBbs

readInput :: Text -> ServerM ExampleApi ([BuiltinByteString], InputContext)
readInput = pure . (,def) . map (stringToBuiltinByteString . T.unpack) . T.splitOn ","
```

### Example server commands

This library includes the [example-server](https://github.com/encryptedcoins/cardano-server/blob/main/src/Cardano/Server/TestingServer/Main.hs) which is the simplest backend application that can be built on top of cardano-server. You can use it as a starting point for your own app development. Here is how to use the example-server and example-client.

1. Run example server which works with test tokens:</br>
```console
$ cabal run cardano-server-example
```

2. Run client in automatic mode in which it will send request to mint test tokens to selected endpoint (the default is `serverTx`) at an average *interval* seconds :</br>
```console
$ cabal run cardano-server-client-example  -- [ping | funds | serverTx | newTx | submitTx | status ] --auto interval
```
&emsp;&emsp;For example:
```console
$ cabal run cardano-server-client-example -- submitTx --auto 30
```

3. Run client in manual mode in which it will send request to selected endpoint (the default is `serverTx`) based on text input:</br>
```console
$ cabal run cardano-server-client-example  -- [ping | funds | serverTx | newTx | submitTx | status ] --manual some_text_input
```
&emsp;&emsp;For example, in serverTx endpoint it will send request to mint specified tokens:
```console
$ cabal run cardano-server-client-example -- serverTx --manual a72kf,jr82ar4
```
