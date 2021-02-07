---
title: "LDA related images"
author: "Seohyun Kim"
date: "2/6/2021"
---

Latent dirichlet allocation (LDA; Blei et al., 2003) is one of topic models that can be helpful in summarizing latent themes in texts. In this model, a topic is a multinomial distribution over the vocabulary. Topics can be interpreted by using words with high probabilities. For example, Let's see the following Figure. I conducted an LDA analysis using students' responses to an essay item. Four topics seemed to be appropriate for this data. The following figure visually presents probabilities over 24 words for the four topics (of course I had more than 24 words). The horizontal axis shows the 24 words. A darker tile indicates higher probability. As shown in the figure with darker tiles, the four topics are characterized by different list of words except for a few words (e.g., w1 to w3).

<img src="https://github.com/Kim-s-h/myRplots/blob/master/LDA/2021-02-01-EMIP-LDA-tile.png" width=300 align=center>



LDA considers each document as a mixture of topics. With LDA, we can estimate which topic dominates each document. In the example data, examinees' responses were scored with from 0 to 4 points. The figure below shows average topic proportions for documents with different points. Responses with different points tended to have different mixtures of topics. 


![Image of topics](https://github.com/Kim-s-h/myRplots/blob/master/LDA/2021-02-01-EMIP-LDA-white.png)