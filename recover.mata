mata:
real matrix recover(real matrix data) {
	real scalar    v, p, colsData, numPanel, firstIndex, lastIndex, firstValue, missCount
	real colvector missIndex
	real rowvector panelStat
	real matrix    dataRecovered, panelInfo, dataTemp, subMatrix

	colsData      = cols(data)
	dataRecovered = sort(data, 1)
	panelInfo     = panelsetup(dataRecovered, 1)
	panelStat     = panelstats(panelInfo)
	numPanel      = panelStat[1]
	
	for (v = 2; v <= colsData; v++) {
		dataTemp = dataRecovered[, (1, v)]
		_sort(dataTemp, (1, 2))
		
		for (p = 1; p <= numPanel; p++) {
			firstIndex = panelInfo[p, 1]
			lastIndex  = panelInfo[p, 2]

			subMatrix  = panelsubmatrix(dataTemp, p, panelInfo)[, 2]
			firstValue = subMatrix[1]

			missIndex  = selectindex(subMatrix :== .)
			missCount  = length(missIndex)
			subMatrix[missIndex] = J(missCount, 1, firstValue)
			dataRecovered[(firstIndex::lastIndex), v] = subMatrix
		}
	}
	return(dataRecovered)
}
end
