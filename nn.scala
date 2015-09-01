import java.sql.{Array=>SQLArray,_}
import scala.collection.mutable.ListBuffer;

object NN
{
	var wordids:List[Int] = List();
	var linkids:List[Int] = List();
	var hiddenids:List[Int] = List();
	
	var ai:List[Double]= List();
	var ah:List[Double]= List();
	var ao:List[Double]= List();
	
	var si:Array[Array[Double]] = Array(Array());
	var so:Array[Array[Double]] = Array(Array());
	
	def main(args:Array[String]){
		createTables();
		
		val players = 101
		val football = 102
		val NBA = 103
		
		val NBAPlayerIntroduction = 201
		val FootballSkills= 202
		val VideoGames = 203
		
		for ( i <- 0 until 30 ) {
			train(List(players,NBA),List(NBAPlayerIntroduction,FootballSkills,VideoGames),NBAPlayerIntroduction)
			train(List(football,NBA),List(NBAPlayerIntroduction,FootballSkills,VideoGames),FootballSkills)
			train(List(players),List(NBAPlayerIntroduction,FootballSkills,VideoGames),VideoGames)
		}
		
		setupNetwork(List(players,NBA),List(NBAPlayerIntroduction,FootballSkills,VideoGames))
		feedForword()
		println(ao)
		
		setupNetwork(List(football,NBA),List(NBAPlayerIntroduction,FootballSkills,VideoGames))
		feedForword()
		println(ao)
		
		setupNetwork(List(NBA),List(NBAPlayerIntroduction,FootballSkills,VideoGames))
		feedForword()
		println(ao)
	}
	
	def createTables(){
		val sqllist = List("create table if not exists hiddennode(create_key)",
						"create table if not exists wordhidden(fromid,toid, strength)",
						"create table if not exists hiddenlink(fromid,toid, strength)" )
		execTran(sqllist)
	}
	
	def setupNetwork(wordids:List[Int],linkids:List[Int]) = {
		this.wordids =  wordids
		this.linkids = linkids
		this.hiddenids = getAllHiddenIds()
		
		ai = wordids.map(x=>1.0)
		ah = hiddenids.map(x=>1.0)
		ao = linkids.map(x=>1.0)
		
		si ={ for ( i <- 0 until wordids.length )
				yield {
					for ( j <- 0 until hiddenids.length )
						yield getStrength(wordids(i),hiddenids(j),0)
					}.toArray
			}.toArray
		so = { for ( i <- 0 until hiddenids.length )
				yield {
					for ( j <- 0 until linkids.length )
						yield getStrength(hiddenids(i),linkids(j),1)
				}.toArray
			}.toArray
	}
	
	def feedForword() = {
		this.ah = 
			{for ( i <- 0 until hiddenids.length )  yield {
				var sum = 0.0
				for ( j <- 0 until wordids.length ) {
					sum += ai(j) * si(j)(i)
				}
				java.lang.Math.tanh(sum)
			}}.toList 
		
		this.ao = 
			{for ( i <- 0 until linkids.length ) yield {
				var sum = 0.0
				for( j <- 0 until hiddenids.length ) {
					sum += ah(j) * so(j)(i)
				}
				java.lang.Math.tanh(sum)
			}}.toList
		
	}
	
	def getAllHiddenIds() = {
		val list1 = this.wordids.map(x=>getHiddenId(x,0))
		val list2 = this.linkids.map(x=>getHiddenId(x,1))
		val list3 = list1.flatMap(x=>x)
		val list4 = list2.flatMap(x=>x)
		(list3:::list4).removeDuplicates
	}
	
	def getHiddenId(id:Int,layer:Int) = {
		var table = "";
		var hidden = "";
		var other = "";
		if( layer == 0 ) {
			table = "wordhidden"
			hidden = "toid"
			other = "fromid"
		}
		if( layer == 1 ) {
			table = "hiddenlink"
			hidden = "fromid"
			other = "toid"
		}
		query("SELECT "+hidden+" FROM "+table+" WHERE "+other+"="+id){ res =>
			bmap(res.next()){
				res.getInt(1)
			}
		}
	}
	
	def backPropagation( targets:List[Double], howMuchToChange:Double = 0.5 ) {
		val outDelta = {
			for ( i <- 0 until linkids.length ) yield {
				//The difference between expectations and real output
				val error = targets(i) - ao(i)
				
				//The reason of caculating slope of output is, when the output is larger, the average strength of links between hidden layer and the output nodes is more strong.
				//So we need to change the strength more careful when the slope is not sheer
				dtanh(ao(i)) * error 
			}
		}.toList
		
		val hiddenDelta = {
			for ( i <- 0 until hiddenids.length ) yield {
				val error = (for ( j <- 0 until linkids.length ) yield so(i)(j) * outDelta(j)).toList.reduceLeft( _ + _ )
				dtanh(ah(i)) * error 
			}
		}.toList
		
		for ( i <- 0 until hiddenids.length ) {
			for ( j <- 0 until linkids.length ) {
				val change = outDelta(j) * ah(i)
				so(i)(j) += change * howMuchToChange
			}
		}
		
		for ( i <- 0 until wordids.length ) {
			for ( j <- 0 until hiddenids.length ) {
				val change = hiddenDelta(j) * ai(i)
				si(i)(j) += change * howMuchToChange
			}
		}
	}
	
	def train(words:List[Int], links:List[Int], selectUrl:Int) {
		checkAndCreateHiddenNode(words,links)
		setupNetwork(words,links)
		feedForword()
		
		val targets = { 
						for (i <- 0 until links.length) yield {
							if (links(i)==selectUrl) 1.0
							else 0.0
						} 
					  }.toList
		backPropagation(targets)
		//Save to database
		saveNetwork()
	} 
	
	//Slope of tanh
	//When y approach to 1, the slope is 0 degree
	//When y approach to 0, the slope is 45 degree
	def dtanh( y:Double ) = 1 - y * y
	
	def saveNetwork() {
		for ( i <- 0 until wordids.length )
			for ( j <- 0 until hiddenids.length )
				setStrength(wordids(i),hiddenids(j),0,si(i)(j))
		
		for ( i <- 0 until hiddenids.length )
			for ( j <- 0 until linkids.length )
				setStrength(hiddenids(i),linkids(j),1,so(i)(j))
	}
	

	
	def getStrength(fromid:Int,toid:Int,layer:Int):Double= layer match {
		case 0=> getStrengthFor(fromid,toid,"wordhidden",-0.2)
		case 1=> getStrengthFor(fromid,toid,"hiddenlink",0.0)
	}
	
	def setStrength(fromid:Int,toid:Int,layer:Int,strength:Double) = layer match {
		case 0=>setStrengthFor(fromid,toid,"wordhidden",strength)
		case 1=>setStrengthFor(fromid,toid,"hiddenlink",strength)
	}
	
	def checkAndCreateHiddenNode(words:List[Int],links:List[Int]) = {
		val wordids = words.sortWith(_ < _)
		val createkey:String = wordids.map(_.toString).reduceLeft(_+"_"+_)
		val exist = query("SELECT * FROM hiddennode WHERE create_key='"+createkey+"'") { res => 
				res.next()
			}
		if( exist==false ) {
			createHiddenNode(wordids:List[Int],links:List[Int],createkey:String)
		}
			
	}
	
	def getStrengthFor(fromid:Int,toid:Int,table:String,default:Double) = {
		query("SELECT strength FROM "+table+" WHERE fromid="+fromid+" AND toid="+toid){ res=>
				if (res.next()) 
					res.getDouble(1) 
				else
					default
			}
	}
	
	def setStrengthFor(fromid:Int,toid:Int, table:String, strength:Double) = {
		if ( existLayerLink(fromid,toid,table) )
			execTran(List("UPDATE "+table+" SET strength="+strength+" WHERE fromid="+fromid+" AND toid="+toid))
		else 
			execTran(List("INSERT INTO "+table+"(fromid,toid,strength) VALUES("+fromid+","+toid+","+strength+") "))
	}
	
	def existLayerLink(fromid:Int,toid:Int,table:String)  = {
		query("SELECT * FROM "+table+" WHERE fromid="+fromid+" AND toid="+toid) { res=>
			res.next()
		}
	}
	
	def createHiddenNode(wordids:List[Int],links:List[Int],createkey:String) {
		execTran(List("INSERT INTO hiddennode(create_key) VALUES('"+createkey+"')"))
		val nodeid = query("SELECT rowid FROM hiddennode WHERE create_key='"+createkey+"'"){ res=>
			res.next()
			res.getInt(1)	
		}
		wordids.foreach( setStrength(_, nodeid, 0, 1.0/wordids.length) )
		links.foreach( setStrength(nodeid, _, 1, 0.1) )
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
		return DriverManager.getConnection("jdbc:sqlite:nn.db")
	}

}