# Deploying a datomic app

## Export AWS credentials

```
$ export AWS_ACCESS_KEY_ID=xxx
$ export AWS_SECRET_KEY=xxx
```

## Create dynamo db table and database

Copy ddb transactor properties and fill in configurations as desired. Then run
the ensure-transactor command, which will create the necessary DDB tables in the
cloud, and also output a revised version of the properties, with information
such as connection credentials etc..
```
$ cp config/samples/ddb-transactor-template.properties my-transactor.properties
$ bin/datomic ensure-transactor my-transactor.properties my-ensured-transactor.properties
```

You can now run the transactor locally connected to the remote DDB
storage service. By running the datomic shell you can create a
database.

```
$ bin/transactor my-ensured-transactor.properties
// => Prints out the uri to connect to.
$ bin/shell
  datomic % uri = "datomic:ddb://eu-central-1/sheet-bucket/sheet-bucket?aws_access_key_id=xxx&aws_secret_key=xxx;
  // => <datomic:ddb://eu-central-1/<ddb-table-name>/<db-name>?aws_access_key_id=AKIAIFFDSGC6V23DWRIA&aws_secret_key=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>
  datomic % Peer.createDatabase(uri);
  <true>
```

## Running the transactor on an EC2 instance

Copy the cf-template properties, edit them as desired, then run the ensure-cf command to setup a CloudFormation template for the transactor.

```
cp config/samples/cf-template.properties my-cf.properties
bin/datomic ensure-cf my-cf.properties my-ensured-cf.properties
bin/datomic create-cf-template my-ensured-transactor.properties my-ensured-cf.properties my-cf.json
```

Then create the CloudFormation stack, it should be up and running in a few minutes! -- Actually, took about 1h15 last time.
```
bin/datomic create-cf-stack eu-central-1 Transactor my-cf.json
```

## Connecting to the remote Transactor

Back in the datomic shell, you can now connect to the same URI as
before, and it will find the cloud transactor:

```
  datomic % Peer.createDatabase(uri);
```

## Destroying the CF stack

```
bin/datomic delete-cf-stack eu-central-1 Transactor
```

## Deploying in T2.micro

I had to edit the cf.json file by hand. Added a mapping: `"t2.micro":{"Arch":"64h"}`, and set the java Xmx to `"Default": "800m"`
`
