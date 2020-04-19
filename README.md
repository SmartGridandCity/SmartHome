# SmartHome

Our study proposes a prediction method that consumes few memory space and without any knowledge requirement. The method is based on sequence prediction. This method decomposes a time series into various sequences, then build a trie to forecast the suffix for a word.

The proposed approach is composed of three steps before the prediction: 
* Dataset: first at all, time series are decomposed into sequences which them are sampled.
* Clustering: due to a large number of different value, a adapted clustering is applied to the sample.
* Sequence prediction: thirdly, the sequences are input of various sequence prediction machine.

We define three main applications of our method:
* Consumption forecast: Here the sequence prediction machine is used for its primary purpose, i.e. to predict the suffix of a given word. 
* Consumption scheduling: The consumption of an appliance is decomposed into several sequences. Those sequences represents various cycle of the appliance. One may use those cycle to create a scheduling of its appliances to determine its future consumptions.
* Demand-side management: As the scheduling, one may determine the effect of demand-side management strategies on the sequences. It is therefore possible to create strategies to reduce consumption, switch off certain devices or reduce consumption of appliances. Note that for these strategies, the consumption sequences can be distorted: by adding a stop time in the sequence, by spreading the consumption curve, by reducing the initial consumption while keeping the pace of the curve, etc.
\end{enumerate}
