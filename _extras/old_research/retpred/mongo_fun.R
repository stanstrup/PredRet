## Append data from data.frame to a document ###########################


# Convert data.frame to bson
bson_data = dataframe2bson(data)


# add to table
mongo <- mongo.create()
ns <- "test.rtdata"

mongo.insert.batch(mongo, ns, bson_data)

del <- mongo.disconnect(mongo)
del <- mongo.destroy(mongo)





## Read all data back and coerse to data.frame ###########################
mongo <- mongo.create()
ns <- "test.rtdata"

data_back = mongo.find.all(mongo, ns,data.frame=T)

del <- mongo.disconnect(mongo)
del <- mongo.destroy(mongo)





## Read only some data ###########################

mongo <- mongo.create()
ns <- "test.rtdata"

# make the query. the requirement
query = mongo.bson.buffer.create()
# requirement 1
mongo.bson.buffer.append(query, "system_name", "LIFE_new")

# requirement 2
mongo.bson.buffer.start.object(query, "rt")
mongo.bson.buffer.append(query, "$lte", 2)
mongo.bson.buffer.finish.object(query)

# finish query
query = mongo.bson.from.buffer(query)


# select fields (think columns)
fields = mongo.bson.buffer.create()
mongo.bson.buffer.append(fields, "system_name", 1L)
mongo.bson.buffer.append(fields, "name", 1L)
mongo.bson.buffer.append(fields, "rt", 1L)
fields = mongo.bson.from.buffer(fields)



data_back = mongo.find.all(mongo, ns=ns,data.frame=T,query=query,fields=fields)




del <- mongo.disconnect(mongo)
del <- mongo.destroy(mongo)













## Read only some data, one item at a time ###########################

mongo <- mongo.create()
ns <- "test.rtdata"

# make the query. the requirement
query = mongo.bson.buffer.create()
# requirement 1
mongo.bson.buffer.append(query, "system_name", "LIFE_new")

# requirement 2
mongo.bson.buffer.start.object(query, "rt")
mongo.bson.buffer.append(query, "$lte", 2)
mongo.bson.buffer.finish.object(query)

# finish query
query = mongo.bson.from.buffer(query)


# select fields (think columns)
fields = mongo.bson.buffer.create()
mongo.bson.buffer.append(fields, "system_name", 1L)
mongo.bson.buffer.append(fields, "name", 1L)
mongo.bson.buffer.append(fields, "rt", 1L)
mongo.bson.buffer.append(fields, "_id", 1L)
fields = mongo.bson.from.buffer(fields)


test = mongo.find.all2(mongo=mongo, ns=ns, query=query, fields=fields,   data.frame=T, mongo.oid2character = T)


del <- mongo.disconnect(mongo)
del <- mongo.destroy(mongo)











?mongo.oid.from.string


















## Read only some data, change it and write back in the same place ###########################
mongo <- mongo.create()
ns <- "test.rtdata"


# Make criteria for data selection
buf <- mongo.bson.buffer.create()
mongo.bson.buffer.append(buf, "name", "1-methyluric acid")
criteria <- mongo.bson.from.buffer(buf)

# set rt to 3
buf <- mongo.bson.buffer.create()
mongo.bson.buffer.start.object(buf, "$set") # $inc works only with one record (ie. not setting mongo.update.multi), $mul doesn't work
mongo.bson.buffer.append(buf, "rt", 3)
mongo.bson.buffer.finish.object(buf)
objNew <- mongo.bson.from.buffer(buf)


# Do the update
mongo.update(mongo, ns, criteria, objNew,flags=mongo.update.multi)
del <- mongo.disconnect(mongo)
del <- mongo.destroy(mongo)





## See result
mongo <- mongo.create()
ns <- "test.rtdata"

# only entries defined in criteria
temp=mongo.find.all(mongo, ns=ns, query=criteria)


# all data
data_back = mongo.find.all(mongo, ns,data.frame=T)
View(data_back[      order(data_back[,"name"])     ,])

del <- mongo.disconnect(mongo)
del <- mongo.destroy(mongo)





















