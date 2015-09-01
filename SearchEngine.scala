import scala.util.matching.Regex
import scala.collection.mutable.ListBuffer 
import java.sql.{Array=>SQLArray,_}

object SearchEngine {
	
	def main(args:Array[String]) {
		queryMatchRows("python programming").foreach(println)
	}
	
	def queryMatchRows(input:String):List[List[String]] = {
		val words:List[String] = split(input)
		val wordIds:List[String] = words.map(queryWordId)
		if(!wordIds.forall(""!=)) {
			println("No record found.")
			return List()
		}
		val sql = constructQueryFindUrlContainAllWord(wordIds)
		query(sql) { 
			rs:ResultSet=>{
				bmap(rs.next) {
					val rows = for( i <- 1 to words.length + 1 ) yield rs.getString(i)
					rows.toList
				}
			}
		}
	} 
	
	//formula PR(A) = 0.15 + 0.85 * (PR(P1)/Links(P1)+...+PR(Pn)/Links(Pn))
	//initial pagerank for every page is 1.0
	//0.85 is damping factor(The probility a person continuing to click on a page), 0.15 is a minimun value which is (1-0.85)
	//The more iteration, the more accurate the pagerank is. but 20 iterations is enough
	def initDataAndCaculatePageRank(iternum:Int = 20) {
		initDataForPageRank()
		caculatePageRank(iternum)
	}
	
	def initDataForPageRank() = {
		val sqllist = List("drop table if exists pagerank",
						"create table pagerank (urlid primary key, score)",
						"insert into pagerank select rowid,1.0 from urllist") 
		execTran(sqllist)
	}
	
	def caculatePageRank(iternum:Int) = {
		for( i <- 0 until iternum )
			getAllUrlID.foreach(caculatePageRankFor(_))
	}
	
	def caculatePageRankFor(urlid:String) = {
		var result = 0.15
		val dampfactor = 0.85
		
		val fromIdList = query("SELECT fromid FROM link WHERE toid="+urlid) { rs=>
			bmap(rs.next) {
				rs.getString("fromid")
			}
		}
		
		fromIdList.foreach ( 
			fromid=>{
				val score = query("SELECT score FROM pagerank WHERE urlid="+fromid)(rs => rs.getDouble("score"))
				val links = query("SELECT count(*) FROM link WHERE fromid="+fromid)(rs => rs.getInt(1))
				result += dampfactor * (score / links)
			}
		)
		execTran(List("UPDATE pagerank SET score="+result+" WHERE urlid="+urlid))
	}
	
	def getPageRankScore(rows:List[List[String]]) = {
		val scorelist:Map[String,Double] = rows.map( row=>Pair(row.head,getPageRankScoreFor(row.head)) ).toMap
		normalize(scorelist)
	}
	
	def getPageRankScoreFor(urlid:String) = {
		query("SELECT score FROM pagerank WHERE urlid="+urlid){rs=>
			rs.getDouble(1)
		}
	}
	
	def getAllUrlID() = {
		query(" SELECT urlid FROM pagerank "){ rs=>
			bmap(rs.next){
				rs.getString("urlid")
			}
		}
	}
	
	def normalize(rows:Map[String,Double],smallIsBetter:Boolean = false ) =  {
		val sorted = rows.toList.sort(_._2 > _._2)
		val verysmall = 0.0000001
		val max = sorted.head._2
		val min = sorted.last._2
		
		smallIsBetter match {
			case true => {
				sorted.map(i=>Pair(i._1,min/Math.max(verysmall,i._2)))
			}
			case false => {
				sorted.map(i=>Pair(i._1,i._2/max))
			}
		}
	}
	
	def constructQueryFindUrlContainAllWord(wordIds:List[String]) = {
		val fields = constructQueryFields(wordIds)
		val tables = constructQueryTables(wordIds)
		val joins = constructQueryJoins(wordIds)
		val conditions = constructQueryCondition(wordIds)
		
		"SELECT "+fields+" FROM "+tables+" WHERE "+joins+" AND "+conditions+" limit 100000"
	}
	
	def constructQueryFields(wordIds:List[String]) = {
		if (wordIds.length == 1)
			" urlid,location "
		else
			wordIds.foldLeft(" w"+wordIds(0)+".urlid")(_+","+"w"+_+".location ")
	}
	
	def constructQueryTables(wordIds:List[String]) = {
		if (wordIds.length == 1 )
			" wordlocation as w"+wordIds(0)
		else
			wordIds.tail.foldLeft("wordlocation as w"+wordIds.head)(_+","+"wordlocation as w"+_)
	}
	
	def constructQueryJoins(wordIds:List[String]) = {
		if (wordIds.length == 1)
			" 1=1 "
		else {
			var joins = ""
			for ( i <- 0 until wordIds.length-1 ) 
				joins += "w"+wordIds(i)+".urlid="+"w"+wordIds(i+1)+".urlid"+" AND "
			joins += " 1=1 "
			joins
		}
	}
	
	def constructQueryCondition(wordIds:List[String]) = {
		var conditions = ""
		wordIds.foreach(x=> conditions += "w"+x+".wordid="+x+" AND ")
		conditions+= " 1=1"
		conditions
	}
	
	def split(input:String) = {
		List.fromString(input, ' ' )
	}
	
	def queryWordId(word:String) = {
		val sql = "SELECT rowid FROM wordlist WHERE word='"+word+"'"
		
		query(sql) { rs:ResultSet=>
			if ( rs.next() )
				rs.getString("rowid")
			else 
				""
		}
	}
	
	def query[T](sql:String)(f: ResultSet => T ):T = {
		val conn = getConnection()
		val stat = conn.createStatement()
		val rs = stat.executeQuery(sql)
		
		val result = f(rs)
		
		rs.close
		stat.close
		conn.close
		return result
	}
	
	def execTran(sqllist:List[String]) = {
		val conn = getConnection()
		val stat = conn.createStatement()
		conn.setAutoCommit( false )
		sqllist.foreach(stat.executeUpdate)
		conn.commit
		conn.setAutoCommit( true )
		stat.close
		conn.close
	}
	
	def bmap[T](test: =>Boolean )(block: =>T ):List[T] = {
		val ret = new ListBuffer[T]
		while(test) ret+=block
		ret.toList
	}
	
	def getConnection():Connection = {
		Class.forName("org.sqlite.JDBC")
		return DriverManager.getConnection("jdbc:sqlite:searchindex.db")
	}
}


