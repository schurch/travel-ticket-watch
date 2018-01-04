$(function () {
    $('#datepicker').datepicker();
    $('#datepicker').datepicker('option', 'dateFormat', 'DD, d MM, yy');
    $('#inputForm').submit(function () {
        $(this).find("input[name='date']").remove();

        var date = $('#datepicker').datepicker('getDate');
        var formattedDate = $.datepicker.formatDate('yy-m-d', date);

        $(this).append('<input type=hidden name=date value=' + formattedDate + ' />');

        return true;
    });
});

function handlerForInputName(inputName) {
    return {
        source: function (request, response) {
            $.ajax({
                url: "/airports/filter",
                data: {
                    term: request.term
                },
                success: function (data) {
                    var results = data.map(airport => ({
                        label: airport.name + " (" + airport.iata + ")",
                        value: airport.name + " (" + airport.iata + ")",
                        iata: airport.iata
                    }));
                    response(results);
                }
            });
        },
        delay: 500,
        minLength: 2,
        select: function (event, ui) {
            var iata = ui.item.iata;
            $('#inputForm').find('input[name=' + inputName + ']').remove();
            $('#inputForm').append('<input type=hidden name=' + inputName + ' value=' + iata + ' />');
        }
    };
}

$(function () {
    $("#fromAirportTextInput").autocomplete(handlerForInputName("from"));
    $("#toAirportTextInput").autocomplete(handlerForInputName("to"));
});