library(lubridate)
library(RODBC);
library(sqldf);

setwd('D:/MySVN/UA-Research/Dr Xia/R Code/TTR_Interpolation/TTR_Interpolation');


Database.Open.Sqlite = function()
{
    Connection = dbConnect(RSQLite::SQLite(), dbname = 'hot_od.sqlite');
    return(Connection);
}

Database.Open.SqlServer = function()
{
    Connection = odbcConnect('TaxiData', uid = 'Shu', pwd = 'Sql.slu2012');
    return(Connection);
}

Connection = Database.Open.Sqlite();

Name = c('a', 'b', 'c', 'd');
Number = 3:6;
Content = data.frame(Name, Number);

dbWriteTable(conn = Connection, name = 'Content', value = Content);
