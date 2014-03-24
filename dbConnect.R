conn<-function(schemaName=NULL){
   return (dbConnect(MySQL.driver, user='user', dbname=schemaName, 
                   host='host', password='password'))
}
