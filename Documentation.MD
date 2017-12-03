# Word Prediction Application
## Author: Sandheep Sebastian
### November 25, 2017



## Summary
This app predicts the next word of a sentence. The prediction algorithm was compiled by going though large amount of twitter, blogs and news text corpus and gathering information about various word fomation and relationships.This application serves as the final capstone project for the Data Specialiation course.
N gram methodology was used to identify word relationships to construct single word frequencies, two word frequencies and three word frequencies and stored in dataframes.Katz's Backoff algorithm was used to predict the probabilities of each of terms in dataframe, In case of unseen observations, the algorithm backed off from the current model to a lower model to pick the highest probable word that could complete the sentence entered. 

## Usage
Click on the "Word Prediction App" tab at the top of this page. Please give this a second to load for the first time use. The left panel conmtains the input elements for the application. Enter a sentence in the text box. Select the number of words to predict. As you type, the app produces the highest probably next word(s) in the sentence. The app also charts the probability of the top n words selected from the drop down menu, that could complete the sentence. There is also a word cloud plot, that shows a list of words that could potentially be the next words, with the size of the word in the word cloud corresponding to the frerquency of the word appearing in the data set used to model this algorithm.

## References
The underlying data on which the processing and finally the algorithm was developed can be found in [Capstone Dataset](https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip)

The source code for the shiny app can be found in [Github](https://github.com/sandheepsebastain/CapstoneProject)
