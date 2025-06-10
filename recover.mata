real matrix recover(real matrix from, real matrix varlist) {
	real scalar    v, p, colsFrom, colsVars, rowsData, colsData, numPanel, firstIndex, lastIndex, firstValue, missCount
	real colvector missIndex
	real matrix    data, dataReturn, panelInfo, subMatrix

	data       = from, varlist
	colsFrom   = cols(from)
	colsVars   = cols(varlist)
	colsData   = cols(data)
	rowsData   = rows(data)
	
	dataReturn = data, (1::rowsData)
	_sort(dataReturn, 1 .. (colsFrom + 1))
	panelInfo  = panelsetup(dataReturn, colsFrom)
	numPanel   = rows(panelInfo)

	for (v = 1; v <= colsVars; v++) {
		for (p = 1; p <= numPanel; p++) {
			firstIndex = panelInfo[p, 1]
			lastIndex  = panelInfo[p, 2]
	
			subMatrix  = panelsubmatrix(dataReturn, p, panelInfo)[, colsFrom + v]
			firstValue = subMatrix[1]

			missIndex  = selectindex(subMatrix :== .)
			missCount  = length(missIndex)
			subMatrix[missIndex] = J(missCount, 1, firstValue)
			dataReturn[(firstIndex::lastIndex), colsFrom + v] = subMatrix
		}
	}
	
	_sort(dataReturn, colsData + 1)
	return(dataReturn[, (colsFrom + 1) .. colsData])
}
