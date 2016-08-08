$(document).ready(function() {
    $('button.submit').click(function() {
        var btn = $(this);
        var problemId = btn.data('problem-id');
        $.ajax({
            url: '/gyakurasetsu/submit',
            data: {
                problem_id: problemId,
                timestamp: btn.data('timestamp'),
                executable_name: btn.data('executable-name')
            },
            method: 'POST',
            dataType: 'JSON'
        }).done(function(data) {
            $('#resemblance' + problemId).text(data.resemblance);
        }).fail(function(data) {
            alert('Failed to submit: ' + data.reason);
        });
    })
});