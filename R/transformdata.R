#' Data transformation
#'
#' Function \code{transformdata} transforms data from year,week,rate to week,rate1,...,rateN
#' suitable to use with mem.
#'
#' Yet to be written
#'
#' @name transformdata
#'
#' @param i.data Data frame of input data.
#' @param i.week.first First surveillance week.
#' @param i.week.last Last surveillance week.
#' @param i.max.na.per maximum percentage of na's in a season allowable, otherwise, the season is removed
#'
#' @return
#' \code{transformdata} returns a data.frame where each column has a different season and
#' rownames are the name of the epidemiological week.
#'
#' @examples
#' # Castilla y Leon Influenza Rates data
#' data(flucylraw)
#' # Transform data
#' newdata<-transformdata(flucylraw)$tdata
#' epi<-memmodel(newdata)
#' print(epi)
#' summary(epi)
#' plot(epi)
#'
#' @author Jose E. Lozano \email{lozalojo@@gmail.com}
#'
#' @references
#' Vega T., Lozano J.E. (2004) Modelling influenza epidemic - can we detect the beginning
#' and predict the intensity and duration? International Congress Series 1263 (2004)
#' 281-283.\cr
#' Vega T., Lozano J.E. (2012) Influenza surveillance in Europe: establishing epidemic
#' thresholds by the Moving Epidemic Method. Influenza and Other Respiratory Viruses,
#' DOI:10.1111/j.1750-2659.2012.00422.x.
#'
#' @keywords influenza
#'
#' @export
#' @importFrom sqldf sqldf
#' @importFrom reshape2 dcast
transformdata<-function(i.data,
						i.week.first=40,
						i.week.last=20,
						i.max.na.per=100){

	# Corrections

	if (i.week.first<1) i.week.first<-1
	if (i.week.first>52) i.week.first<-52
	if (i.week.last<1) i.week.last<-1
	if (i.week.last>52) i.week.last<-52
	if (i.week.first==i.week.last) i.week.last<-i.week.last-1

	# set season

	if (i.week.first<i.week.last){
	  #i.data$season<-paste(i.data$year,i.data$year,sep="/")
	  i.data$season<-as.character(i.data$year)
	}else{
	  i.data$season[i.data$week<i.week.first]<-paste(i.data$year[i.data$week<i.week.first]-1,i.data$year[i.data$week<i.week.first],sep="/")
	  i.data$season[i.data$week>=i.week.first]<-paste(i.data$year[i.data$week>=i.week.first],i.data$year[i.data$week>=i.week.first]+1,sep="/")
	}

	# First we create the frame of surveillance weeks
	# The default scheme is: 40,41,...,52,1,...,20
	# Sometimes surveillance period goes from 30,31,...52,1,...,29
	# In southern hemisphere countries, surveillance is the contrary: 15,..,44 or 1,..,52

	if (i.week.first>=i.week.last) week.scheme.52<-data.frame(week=c(i.week.first:52,1:i.week.last,NA)) else week.scheme.52<-data.frame(week=i.week.first:i.week.last)
	week.scheme.52$week.no<-1:dim(week.scheme.52)[1]

	if (i.week.first>=i.week.last) week.scheme.53<-data.frame(week=c(i.week.first:53,1:i.week.last)) else week.scheme.53<-data.frame(week=i.week.first:i.week.last)
	week.scheme.53$week.no<-1:dim(week.scheme.53)[1]

	# Separate seasons with week 53 and seasons without week 53
	# With week 53
	temp1.1<-sqldf("select distinct season from [i.data] where week=53")
	temp2.1<-sqldf("select * from [i.data] where season in (select season from [temp1.1])")
	temp3.1<-sqldf("select [temp1.1].*, [week.scheme.53].* from [temp1.1], [week.scheme.53]")
	temp4.1<-sqldf("select [temp3.1].season, [temp3.1].[week.no], [temp2.1].rate from [temp3.1] left join [temp2.1] on ([temp3.1].season=[temp2.1].season and [temp3.1].week=[temp2.1].week)")
	# Without week 53
	temp1.2<-sqldf("select distinct season from [i.data] where season not in (select season from [temp1.1])")
	temp2.2<-sqldf("select * from [i.data] where season in (select season from [temp1.2])")
	temp3.2<-sqldf("select [temp1.2].*, [week.scheme.52].* from [temp1.2], [week.scheme.52]")
	temp4.2<-sqldf("select [temp3.2].season, [temp3.2].[week.no], [temp2.2].rate from [temp3.2] left join [temp2.2] on ([temp3.2].season=[temp2.2].season and [temp3.2].week=[temp2.2].week)")

	temp4<-rbind(temp4.1,temp4.2)
	temp5<-temp4[order(temp4$season,temp4$week.no),]

	temp6<-dcast(temp5, formula = week.no ~ season,sum,value.var="rate")

	# Is possible that due to constraint of surveillance weeks, some seasons are empty,
	# remove all columns with all na values.

	temp7<-temp6[apply(temp6,2,function(i.data) any(!is.na(i.data)))]

	temp8<-sqldf("select [week.scheme.52].week, temp7.* from temp7 inner join [week.scheme.52] on (temp7.[week.no]=[week.scheme.52].[week.no]) order by [week.no]")

	temp9<-subset(temp8,!is.na(temp8$week))
	row.names(temp9)<-temp9$week
	temp9$week<-NULL
	temp9$week.no<-NULL

	# remove those seasons whose percentaje of NA is greater than a parameter
	
	temp10<-apply(temp9,2,function(x) sum(is.na(x))/length(x))
	temp11<-temp9[temp10<i.max.na.per/100]
	
	transformdata.output<-list(tdata=temp11)
	transformdata.output$call<-match.call()
	return(transformdata.output)

}
