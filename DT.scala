import scala.io._
import scala.math._
import java.text.{NumberFormat,ParsePosition}

object DT {
	val rows = getTrainSet("decision_tree_example.txt")
	def main(arg:Array[String]) {
		//val (set1,set2) = divide(rows,2,"yes")
		//println(entropy(set1))
		
		val tree = buildTree(rows)
		printTree(tree)
		println("------------------------------")
		printTree(tree)
		println("------------------------------")
		println(mdclassify(List("google", "France" ,"", "" ),tree))
		//println(isNumberic("1"))
	}
	
	def mdclassify(item:List[String], dtree:DTreeNode): Option[Map[String,Int]] = {
		if (dtree.result != None)
			return dtree.result
		if (dtree.colIndex==None)
			return None;
			
		if ( item(dtree.colIndex.get) == "" ) {
			val r1 = mdclassify(item,dtree.left.get)
			val r2 = mdclassify(item,dtree.right.get)
			return Some(r1.get ++ r2.get)
		}
		
		if (isNumberic(item(dtree.colIndex.get))) {
			if (item(dtree.colIndex.get).toFloat >= dtree.split.asInstanceOf[Float])
				return mdclassify(item,dtree.left.get)
			else
				return mdclassify(item,dtree.right.get)
		} else {
			if (item(dtree.colIndex.get)==dtree.split.asInstanceOf[String])
				return mdclassify(item,dtree.left.get)
			else
				return mdclassify(item,dtree.right.get)
		}
		return None
	}
	
	def classify(item:List[String], dtree:DTreeNode):Option[Map[String,Int]] = {
		if (dtree.result!=None)
			return dtree.result
		if (dtree.colIndex==None)
			return None
		
		val (set1, set2) = divide(List(item), dtree.colIndex.get , dtree.split)
		if (set1.length!=0) {
			if (dtree.left==None)
				return None
			return classify(item,dtree.left.get)
		}
		
		if (set2.length!=0) {
			if (dtree.right==None)
				return None
			return classify(item,dtree.right.get)
		}
		return None
	}
	
	def buildTree(rows:List[List[String]]):DTreeNode = {
		if (rows.length <= 0 ) 
			return new DTreeNode
		
		val currentScore = entropy(rows)
		
		//The last column is result column, so this column should be excluded
		val colcount = rows(0).length - 1
		
		//Caculate information gain for each column
		val candidates = for ( colIt <- 0 until colcount ) yield {
			val splitValues = rows.map( v => v(colIt) ).distinct
			
			//Caculate information gain for each splitter
			val infgains = for ( split <- splitValues ) yield {
				val (set1,set2) = divide(rows, colIt, split)
				val entropy1 = entropy(set1)
				val entropy2 = entropy(set2)
				val p1 = set1.length.toFloat/rows.length.toFloat
				val p2 = 1.0 - p1
				val average = p1 * entropy1 + p2 * entropy2
				val infgain = currentScore - average
				(split,infgain)
			}
			val bettergain = infgains.sort(_._2 > _._2).head
			(colIt,bettergain._1,bettergain._2)
		}
		
		//find largest information gain
		val best = candidates.toList.sort(_._3 > _._3).head
		if ( best._3 > 0){
			val (set1,set2) = divide(rows,best._1,best._2)
			return new DTreeNode(
				colIndex = Some(best._1),
				split = best._2,
				left = Some(buildTree(set1)),
				right = Some(buildTree(set2))
			)
		}
		else
			return new DTreeNode(
				result = Some(countResult(rows))
			)
	}
	
	def prune(tree:DTreeNode , threshold:Float ):Unit = {
		if (tree.left != None ) 
			prune(tree.left.get, threshold)
		
		if (tree.right != None ) 
			prune(tree.right.get, threshold)
		
		if (tree.left != None && tree.left.get.result!=None && 
		tree.right != None && tree.right.get.result !=None ) {
			val left = tree.left.get.result.get.toList.map(x=> (for(i<-0 to x._2) yield List(x._1)).toList ).flatten
			val right = tree.right.get.result.get.toList.map( x=> (for(i<-0 to x._2) yield List(x._1)).toList ).flatten
			val p1 =  left.length / (left.length + right.length)
			val p2 = 1.0 - p1
			println(entropy(left++right)+":" + entropy(left)*p1+":" + entropy(right)*p2)
			val diff = entropy(left++right) - (entropy(left)*p1 + entropy(right)*p2)
			if ( diff <= threshold ) {
				tree.colIndex = None
				tree.split = None
				tree.left = None
				tree.right = None
				tree.result = Some(countResult(left ++ right))
			}
		} 
	}
	
	def printTree(tree:DTreeNode, indent:String="") {
		if (tree.result!=None)
			println(tree.result.get)
		else {
			println(tree.colIndex.get+":"+tree.split)
			print(indent+"T->")
			printTree(tree.left.get, indent+"    ")
			print(indent+"F->")
			printTree(tree.right.get, indent+"    ")		
		}
	}
	
	def entropy(rows:List[List[String]]):Float = {
		val count = countResult(rows)
		val total = rows.length
		val log2 = (x:Double) => log(x)/ log(2)
		
		val prob = (x:Int) => x.toFloat / total
		
		val parts =  for( c <- count ) yield prob(c._2) * log2(prob(c._2))
		return 0.0f-parts.sum.toFloat
	}
	
	def giniImpurity(rows:List[List[String]]) = {
		val count = countResult(rows)
		val total = rows.length
		val keys = count.keys.toList
		
		var imp = 0.0
		for ( i <- 0 until keys.length )  {
			for ( j <- (i+1) until keys.length ) {
				val x = (count(keys(i)).toFloat / total.toFloat)
				val y = (count(keys(j)).toFloat / total.toFloat)
				imp += x * y
			}
		}
		imp 
	}
	
	def countResult(rows:List[List[String]]) = {
		val result = rows.map(r => r(r.length-1)).distinct
		val count = for( r <- result ) 
				yield (r,rows.filter(row=>row(row.length-1)==r).length)
		
		count.toMap
	}
	
	def divide(rows:List[List[String]], colIndex:Int, splitValue:Any) = {		
		val splitFunc = splitValue match {
			case x if x.isInstanceOf[Int] => (value:String) => value.toInt >= x.asInstanceOf[Int]
			case y if y.isInstanceOf[Float] => (value:String) => value.toFloat >= y.asInstanceOf[Float]
			case z if isNumberic(z.toString) => (value:String) => value.toFloat >= z.toString.toFloat
			case _ => (value:String) => {value==splitValue}
		}

		(rows.filter(r => splitFunc(r(colIndex))),rows.filter(r => !splitFunc(r(colIndex))))
	}
	
	def getTrainSet(file:String) = {
		val lines = Source.fromFile(file).getLines().toList
		lines.map(_.split('\t').toList)
	}
	
	def isNumberic(input:String) = {
		val formatter = NumberFormat.getInstance;
		val pos = new ParsePosition(0)
		formatter.parse(input,pos)
		input.length == pos.getIndex
	}
}

class DTreeNode( var colIndex:Option[Int]= None,
			var split:Any = None,
			var left:Option[DTreeNode] = None, 
			var right:Option[DTreeNode] = None,
			var result:Option[Map[String,Int]] = None ) {
	
}
