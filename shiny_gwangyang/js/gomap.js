// When locator icon in datatable is clicked, go to that spot on the map
$(document).on("click", ".go-map", function(e) {
	e.preventDefault();
	$el = $(this);
	var lat = $el.data("lat");
	var long = $el.data("long");
	var code = $el.data("code");
	console.log(`code=${code}, lat=${lat}, lng=${long}`);
	$($("#nav a")[0]).tab("show");
	Shiny.onInputChange("goto", {
		lat: lat,
		lng: long,
		code: code,
		nonce: Math.random()
	});
});