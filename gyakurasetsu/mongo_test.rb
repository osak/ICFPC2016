require 'mongo'

client = Mongo::Client.new(['localhost:27017'], database: 'origami')
db = client.database

p db[:solutions].find().first['hoge']
