
uniqueValues = function(array, field) {
    var unique = {};
    var distinct = [];
    for(var i in array){
	if(typeof(unique[array[i][field]]) == "undefined"){
	    distinct.push(array[i][field]);
	}
	unique[array[i][field]] = 0;
    }
    return (Object.keys(unique))
}

function isNumeric(n) {
    return !isNaN(parseFloat(n)) && isFinite(n);
}
