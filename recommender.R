library(plyr)

# Comparing and finding similarities between two beers (so as to recommend the most similar beer).
# read the data set, extract useful data and remove duplicates
loadAndCleanupData <- function(filePath){
	# loading the beer data set
	beer_data <- read.csv(filePath, header = TRUE, stringsAsFactors = FALSE)
	colnames(beer_data) <- c("brewery_id", "brewery", "review_time", "review_overall", "review_aroma", "review_appearance", "review_profilename", "beer_style", "review_palate", "review_taste", "beer_name", "beer_abv", "beer_id")

	# subsetting beer data to include only those beers with greater than 500 reviews
	beer_data <- ddply(beer_data, "beer_id", function(x){
		if(nrow(x) > 500){
			return(x)
		}else{
			return()
		}
	})

	# One reviewer should have only one review for a particular beer
	beer_data <- ddply(beer_data, c("beer_id"), function(x){
		dups <- duplicated(x$review_profilename)
		x <- x[!dups, ]
		return(x)
	})

}

# get beer name from beer id provided
getBeerNameFromID <- function(beer_data, beer_id){
	return(beer_data[which(beer_data$beer_id == beer_id), ]$beer_name[1])
}

# get average rating of a particualr beer
getAverageBeerRating <- function(beer_data, beer_id){
	return(round(mean(beer_data[which(beer_data$beer_id == beer_id), ]$review_overall, na.rm = TRUE), 2))
}

# Let us first find a list of beers reviewed by the same person
# function takes two beer ids and finds their mutual reviewer(s)
getMutualReviewerByBeerID <- function(beer_data, id1, id2){
	beer1_data <- subset(beer_data, beer_id == id1)
	beer2_data <- subset(beer_data, beer_id == id2)

	mutual_reviewers <- intersect(beer1_data$review_profilename, beer2_data$review_profilename)
	return(mutual_reviewers)
}

# The function takes two beer names and finds their mutual reviewer(s)
getMutualReviewerByBeerName <- function(beer_data, name1, name2){
	beer1_data <- subset(beer_data, beer_name == name1)
	beer2_data <- subset(beer_data, beer_name == name2)

	mutual_reviewers <- setdiff(beer1_data$review_profilename, beer2_data$review_profilename)
	return(mutual_reviewers)
}


# Now let's identify and extract the features of each beer
getFeatureList <- function(one_beer_data, reviewers_list){
	# identifying features
	features <- c("review_profilename", "review_overall", "review_aroma", "review_appearance", "review_palate", "review_taste")
	# getting data of requested beer
	beer1_data <- unique(subset(one_beer_data, review_profilename %in% reviewers_list))
	# selecting only features
	beer1_data <- beer1_data[features]
	return(beer1_data)
}


# now calculating similarity between two beers
calculateSimilarity <- function(beer_data, beer_id1, beer_id2){
	# overall review should be given more weightage
	weights <- c(2, 1, 1, 1, 1)
	# Note that it is assumed that a particular reviewer would be objective in his reviews
	common_reviewers <- getMutualReviewerByBeerID(beer_data, beer_id1, beer_id2)
	#common_reviewers <- getMutualReviewerByBeerName(beer_data, "Fat Tire Amber Ale", "Dale's Pale Ale")

	# get the two beers' dataset
	beer1_data <- subset(beer_data, beer_id == beer_id1)
	beer2_data <- subset(beer_data, beer_id == beer_id2)
	
	# get feature list of each beer
	beer1_features <- unique(getFeatureList(beer1_data, common_reviewers))
	beer2_features <- unique(getFeatureList(beer2_data, common_reviewers))

	# order by reviewer name
	beer1_features <- beer1_features[order(beer1_features$review_profilename), ]
	beer2_features <- beer2_features[order(beer2_features$review_profilename), ]
	beer1_features$review_profilename <- NULL
	beer2_features$review_profilename <- NULL


	features <- colnames(beer1_features)
	# calculating pearson coefficient
	# warning: takes a long time
	correlations <- sapply(features, function(metric){
		cor(beer1_features[metric], beer2_features[metric])
	})

	correlations <- sum(correlations * weights, na.rm = TRUE)
	return(correlations)
}

# calculate similarity for all beer pairs
calculateSimilarityMatrix <- function(beer_data){
	# creating all possible beer pairs
	beer_pairs <- expand.grid(beer1 = unique(beer_data$beer_id), beer2 = unique(beer_data$beer_id))
	beer_pairs <- subset(beer_pairs, beer1 != beer2)

	# calculating the similarity coefficient (pearson coefficient) for all possible beer pairs
	similarityList <- apply(beer_pairs, 1, function(x){
		print(paste(x[['beer1']], x[['beer2']]))
		x[['similarity_coefficient']] <- calculateSimilarity(beer_data, x[['beer1']], x[['beer2']])
		return(x)
	})

	# converting to useful format
	similarityMatrix <- as.data.frame(t(similarityList))

	return(similarityMatrix)
}

# extracting recommendations matrix from similarity matrix
createRecommendationMatrix <- function(similarityMatrix){
	recommendationMatrix <- ddply(similarityMatrix, "beer1", function(x){
		x <- x[order(as.numeric(x$similarity_coefficient), decreasing = TRUE), ]
		x <- head(x, 3)
		return(x)
	})

	return(recommendationMatrix)
}

# get recommendations for a particular beer id
getBeerRecommendations <- function(beer_data, similarityMatrix, beer_id){
	recommendationMatrix <- createRecommendationMatrix(similarityMatrix)
	#write.csv(recommendationMatrix, "~/projects/Beer Recommendations/recommendationMatrix.csv", row.names = F, quote = F)
	recommendationMatrix <- ddply(recommendationMatrix, "beer2", transform, rec_beer_name = getBeerNameFromID(beer_data, beer2[1]))
	recommendationMatrix <- ddply(recommendationMatrix, "beer2", transform, rec_beer_rating = getAverageBeerRating(beer_data, beer2[1]))

	recommendations <- subset(recommendationMatrix, beer1 == beer_id)
	return(recommendations)
}


filePath <- "~/projects/Beer Recommendations/data/beer_reviews.csv"
beer_data <- loadAndCleanupData(filePath)
print(head(beer_data))

similarityMatrix <- calculateSimilarityMatrix(beer_data) 
#write.csv(calculateSimilarityMatrix(beer_data), "~/projects/Beer Recommendations/similarityMatrix.csv", row.names = F, quote = F)


print(getBeerRecommendations(beer_data, similarityMatrix, 6))