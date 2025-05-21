mata:
real matrix recover(real colvector from, real matrix varlist) {
	real scalar    v, p, rowsData, colsData, numPanel, firstIndex, lastIndex, firstValue, missCount
	real colvector missIndex
	real rowvector panelStat
	real matrix    data, dataRecovered, panelInfo, dataTemp, subMatrix

	data          = from, varlist
	colsData      = cols(data)
	rowsData      = rows(data)
	dataRecovered = data, (1::rowsData)
	_sort(dataRecovered, 1)
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
	_sort(dataRecovered, (colsData + 1))
	return(dataRecovered[, 2 .. colsData])
}
end
