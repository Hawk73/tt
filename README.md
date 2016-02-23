traffic
=========

## Install

Create release

```
git clone git@github.com:Hawk73/tt.git

cd tt/apps/traffic

make
...
===> Starting relx build process ...
===> Resolving OTP Applications from directories:
          /tt/apps/traffic/ebin
          /tt/apps/traffic/deps
          /usr/local/Cellar/erlang/18.2.1/lib/erlang/lib
          /apps/traffic/apps
===> Resolved traffic_release-1.0.0
===> Including Erts from /usr/local/Cellar/erlang/18.2.1/lib/erlang
===> release successfully created!
```

Start server:
```
make on
```
Stop server:
```
make off
```

You can view logs by following path:
```
app/traffic/_rel/traffic_release/log/
```


## Test

```
make test
```
Successful result:

```
...
Testing apps.traffic.traffic_SUITE: TEST COMPLETE, 4 ok, 0 failed of 4 test cases
...
GEN    eunit
  All 14 tests passed.
```

## Curl commands

- Create sequence
```
curl -v -X POST -d '{}' localhost:8080/sequence/create
```
- Send indication
```
curl -v -X POST -d '{"observation": {"color": "green", "numbers": ["1110111", "0011101"]}, "sequence": "b839e67c-d637-4afc-9241-63943c4fea83"}' localhost:8080/observation/add
```
- Clear all data
```
curl -v -X POST -d '{}' localhost:8080/clear
```

## Other

- You can change HTTP server port in
```
tt/apps/traffic/include/server_config.hrl
```
