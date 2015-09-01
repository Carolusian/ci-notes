import java.util.Random

object ProgramTree{
	val seed = new Random(System.currentTimeMillis())
	implicit val testdata = generateData()
	
	def main(args:Array[String])={
		val n = new Node(ifWapper,
				List(Node(greatWapper,List(ParamNode(0),ConstNode(5))),
					Node(addWapper,List(ParamNode(1),ConstNode(3))),
					Node(subWapper,List(ParamNode(1),ConstNode(2)))))
		//n.displayTree()
		//println(n.evaluate(List(2,3)))
		// val randomTree = makeRandomTree(2)
		//randomTree.displayTree()
		/* val mutatedTree = mutate(randomTree,2)
		mutatedTree.displayTree()
		val randomFather = makeRandomTree(2)
		println("Father is:")
		randomFather.displayTree()
		
		val randomMother = makeRandomTree(2)
		println("Mother is:")
		randomMother.displayTree()
		
		val crossTree = crossover(randomFather,randomMother)
		println("Child is:")
		crossTree.displayTree() */
	
		//testdata.foreach( x => printf("%5d %5d %5d\n",x._1,x._2,x._3) )
		//println("------")
		//println(score(n,testdata.toList))
		val n2 = makeRandomTree(2)
		val l = List(n,n2)
		println(score(n2,testdata.toList))
		println(score(n,testdata.toList))
		rankPrograms(List(n2,n)).foreach(println)
		evolve(2)
		/* val m = new Node(addWapper,
							List(
								Node(),
								Node()
							)
						)*/
	}
	
	def iff(l:List[Int]):Int = {
		if (l(0)>0) l(1)
		else l(2)
	}
	val ifWapper = new FuncWapper(iff,3,"if")

	def gt(l:List[Int]):Int = {
		if (l(0) > l(1)) 1 else 0
	}
	val greatWapper = new FuncWapper(gt,2,">")

	val addWapper = new FuncWapper((l:List[Int])=>l(0)+l(1),2,"+")

	val subWapper = new FuncWapper((l:List[Int])=>l(0)-l(1),2,"-")
	
	val multiWapper = new FuncWapper((l:List[Int])=>l(0)*l(1),2,"*")
	
	val funcList = List(ifWapper,greatWapper,addWapper,subWapper,multiWapper)
	
	def makeRandomTree(paramCount:Int,depth:Int=4,funcProb:Double=0.5,paramProb:Double=0.6) = {
		if ( makeRandomFraction() < 0.5 && depth > 0 )
			makeFuncNode(paramCount,depth,funcProb,paramProb)
		else if (makeRandomFraction() < 0.6)
			makeParamNode(paramCount)
		else 
			makeConstNode()
	}
	
	def makeFuncNode(paramCount:Int,depth:Int,funcProb:Double=0.5,paramProb:Double=0.6):Node = {
		val func = chooseRandomFunc()
		val children = for (i <- 0 until func.childCount) yield (makeRandomTree(paramCount,depth-1,funcProb,paramProb))
		
		return new Node(func,children.toList)
	}
	
	def makeParamNode(paramCount:Int):ParamNode = {
		return new ParamNode(seed.nextInt(paramCount))
	}
	
	def makeConstNode():ConstNode = {
		return new ConstNode(seed.nextInt(10))
	}
	
	def chooseRandomFunc():FuncWapper = {
		funcList(seed.nextInt(funcList.length))
	}
	
	def makeRandomFraction()  = {
		val v = seed.nextDouble
		v
	}
	
/** *****************************************************************************************************
 * mutation and crossover part
 *******************************************************************************************************/
	def mutate(tree:N, paramCount:Int, prob:Double = 0.1 ):N = {
		if ( seed.nextDouble < prob )
			makeRandomTree(paramCount)
		else {
			mutateChildren(tree,paramCount)
		}
	}

	def mutateChildren(parent:N,paramCount:Int) = {
		var result = parent.clone
		if (parent.isInstanceOf[Node])
			result.asInstanceOf[Node].children = parent.asInstanceOf[Node].children.map(mutate(_,paramCount)) 
		result
	}
	
	def crossover(father:N,mother:N,prob:Double = 0.7, top:Boolean = true ):N = {
		if ( seed.nextDouble < prob && !top )
			mother.clone
		else {
			crossoverChildren(father,mother, prob);
		} 
	}
	
	def crossoverChildren(father:N,mother:N,prob:Double):N = {
		var result = father.clone
		if( father.isInstanceOf[Node] && mother.isInstanceOf[Node] ) 
			result.asInstanceOf[Node].children = father.asInstanceOf[Node].children.map(crossover(_,choose(mother.asInstanceOf[Node].children), prob, false ));
		result
	}
	
	def choose(children:List[N]):N = {
		children(seed.nextInt(children.length))
	}
	
/** *******************************************************************************************************************
 * Prepare test dataset
 *********************************************************************************************************************/
	
	//This is the test function. We need our program evolves to get the same result as this function
	def targetFunc(args:List[Int]) = args(0) * args(0) + 2 * args(1) + 3 * args(0) + 5
	
	def generateData():List[(Int,Int,Int)] = {
		val data = for (i <- 0 until 100)  yield  {
			val x = seed.nextInt(50);
			val y = seed.nextInt(50);
			(x,y,targetFunc(List(x,y)))
		}
		println("--------------")
		data.foreach(println)
		println("--------------")
		data.toList
	}
	
	//Test how different the evaluate program and the target program, 0 means they are same.
	def score( program:N, dataset:List[(Int,Int,Int)] ) = {
		val diffList = dataset.map( x=> Math.abs(x._3-(program.evaluate(List(x._1,x._2)))))
		diffList.reduceLeft( (x:Int,y:Int) => x+y )
	}


/** *********************************************************************************************
 * Makes your program evolve
 ************************************************************************************************/
	def evolve(paramCount:Int,popSize:Int=500,genSize:Int=500,probMut:Double=0.1,probCross:Double=0.6,probNew:Double=0.2,probExp:Double=0.7) = {
		val ancestors = (for( i <- 0 until popSize ) yield makeRandomTree(2)).toList
		val last = breed(ancestors,paramCount,popSize,genSize,probNew,probExp)
		last.displayTree()
		last
	}
	
	def breed(ancestors:List[N],paramCount:Int,popSize:Int,genSize:Int,probNew:Double,probExp:Double):N = {
		 var population = ancestors
		 for( i <- 0 until genSize ) {
			val rankedPrograms = rankPrograms(population)
		
			//Display the score of this generation, the first people is the best one
			println(rankedPrograms(0)._2)
			
			//We get the program we want when the score become 0
			if( rankedPrograms(0)._2 == 0 ) 
				return rankedPrograms(0)._1
				
			population = breedOneGeneration(rankedPrograms,paramCount,popSize,probNew,probExp)
		}
		return population(0)
	}
	
	def breedOneGeneration(rankedPrograms:List[(N,Int)],paramCount:Int,popSize:Int,probNew:Double,probExp:Double) = {
		//Always reserve the top 2 best programs
		var newgen = List(rankedPrograms(0)._1,rankedPrograms(1)._1)
		while (newgen.length < popSize ) {
			if (seed.nextDouble > probNew) {
				val eliteFather = rankedPrograms(eliteIndex(probExp))._1
				val eliteMother = rankedPrograms(eliteIndex(probExp))._1
				val child = mutate(crossover(eliteFather,eliteMother),paramCount)
				newgen = newgen:::List(child)
			}
			else { //We need aliens to make the group of people to be diversity. This can avoid local maxima problem
				newgen = newgen:::List(makeRandomTree(paramCount))
			}
		}
		newgen
	}
	
	
	def eliteIndex(probExp:Double) = {
		(Math.log(seed.nextDouble)/Math.log(probExp)).toInt
	}
	
	def rankPrograms(programs:List[N]) = {
		val scoreList = programs.map( (n:N) => (n,score(n,testdata)) )
		val ranked = scoreList.sort((x1,x2)=>Math.abs(x1._2) < Math.abs(x2._2))
		ranked
	}
}


 
class FuncWapper( val func:List[Int]=>Int, val childCount:Int , val name:String )

trait N extends Cloneable {
    def evaluate(params:List[Int]) : Int
	def displayTree( depth:Int = 0 )
	override def clone = {
		super.clone.asInstanceOf[N]
	}
}

case class Node( val fw:FuncWapper, var children:List[N] ) extends N {
    def evaluate(params:List[Int]):Int = {
        val results = children.map(_.evaluate(params))
        fw.func(results)
    }
	
	/* override def clone()={
		val copied = new Node(fw,children)
		copied.children = children.map( x=> x)
		copied
	}*/
	
	private def displayName(depth:Int) {
		for( i <- 0 until depth ) 
			print(" ")
		print(fw.name)
		println()
	}
	
	private def displayChildren(depth:Int) {
		children.foreach(_.displayTree(depth))
	}
	
	def displayTree( depth:Int = 0 ) = {
		this.displayName(depth)
		this.displayChildren(depth+1)
	}
}

case class ParamNode( val parampos:Int ) extends N {
    def evaluate(params:List[Int]):Int = {
        params(parampos)
    }
	
	def displayTree( depth:Int = 0 ) = {
		for( i <- 0 until depth )
			print(" ")
		print("p"+parampos)
		println()
	}
}

case class ConstNode( val v:Int ) extends N {
    //params here is useless
	def evaluate(params:List[Int]) = {
        v
    }
	
	def displayTree( depth:Int = 0 ) = {
		for( i <- 0 until depth )
			print(" ")
		print(v)
		println()
	}
} 