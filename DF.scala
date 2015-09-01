import java.util.regex._

object DF {
	var fc = Map[String,scala.collection.mutable.Map[String,Int]]()
	var cc = scala.collection.mutable.Map[String,Int]()
	
	def main(arg:Array[String]) {
		//train("the quick brown fox jumps over the lazy dog","good")
		//train("make quick money in the online casino","bad")
		train("Nobody owns the water.","good")
		train("the quick rabbit jumps fences","good")
		train("buy pharmaceuticals now","bad")
		train("make quick money at the online casino","bad")
		train("the quick brown fox jumps","good")
		//println(getFeatureProb("quick","good"))
		//println(this.countItemInCate("good"))
		//println(this.countFeatureInCate("quick","good"))
		//println(this.naivebayes( "quick rabbit" , "good" ))
		//println(this.naivebayes( "quick rabbit" , "bad" ))
		println(this.cateProbOnFeature("quick","good"))
		println(this.cateProbOnFeature("money","bad"))
	}
	
	def train(item:String, cate:String) {
		getFeatures(item).foreach(incf(_,cate))
		incc(cate)
	}
	
	def fisher() = {
		
	}
	
	def cateProbOnFeature(feature:String, cate: String) = {
		if (featureInAllCate(feature)==0)
			0
		else 
			getFeatureProb(feature,cate) / cc.keys.map(getFeatureProb(feature,_)).sum
	} 
	
	def featureInAllCate(feature:String) = fc.contains(feature) match {
		case false => 0
		case true => fc(feature).values.sum 
	}
	
	def naivebayes(item:String, cate:String) = {
		getDocProb(item,cate) * countTotalFeaturesInCate(cate) / countTotalFeatures()
	}
	
	def getDocProb(item:String, cate:String) = {
		getFeatures(item).map(getWeightedProb( _, cate )).reduceLeft(_*_)
	}
	
	def getWeightedProb(feature:String, cate:String , weight:Float = 1.0f, assumeProb:Float = 0.5f) = {
		val basicProb = this.getFeatureProb(feature,cate)
		val total = this.fc(feature).values.sum
		
		(weight * assumeProb + basicProb * total) / (total + weight) 
	}
	
	def getFeatureProb(feature:String , cate:String) = countTotalFeaturesInCate(cate) match {
		case 0 => 0
		case _ => countFeatureInCate(feature,cate).toFloat / countTotalFeaturesInCate(cate).toFloat
	}
	
	//Increase feature count for a category
	def incf(feature:String, cate:String) {
		if(!fc.isDefinedAt(feature)) {
			fc += ((feature, scala.collection.mutable.Map[String,Int]()))
		}
		
		if(!fc(feature).isDefinedAt(cate)) {
			fc(feature)+=((cate, 0))
		}
		fc(feature)(cate) += 1
	}
	
	//Increase category count
	def incc(cate:String) {
		if(!cc.isDefinedAt(cate)) {
			cc += ((cate,0))
		}
		cc(cate) += 1
	}
	
	def countFeatureInCate(feature:String , cate:String) = {
		val hasCate = this.fc(feature).filter(_._1==cate);

		hasCate.size match {
			case 0 => 0
			case _ => hasCate(cate)
		}
	}
	
	def countTotalFeaturesInCate(cate:String) = {
		this.cc.contains(cate) match {
			case true => this.cc(cate)
			case _=> 0
		}
	}
	
	def getFeatures(item:String)={
		val pattern = Pattern.compile("\\W")
		pattern.split(item.trim).toList.distinct
	}

	def countTotalFeatures() = {
		this.cc.values.sum
	}
}
