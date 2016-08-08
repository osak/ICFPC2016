function clearLog() {
    $('#information').html('');
    $('#information').removeClass('alert-info alert-warning alert-danger');
}

function log(str) {
    clearLog();
    $('#information').text(str);
    $('#information').addClass('alert-info');
}

function error(str) {
    clearLog();
    $('#information').text(str);
    $('#information').addClass('alert-danger');
}

function drawFacet(ctx, facet, selected, normalizer) {
    ctx.beginPath();
    ctx.strokeStyle = 'black';
    ctx.lineWidth = 1;
    ctx.setLineDash([]);
    {
        var npInit = normalizer(facet[0]);
        ctx.moveTo(npInit.x, npInit.y);
    }
    facet.forEach(function (p, i) {
        var np = normalizer(p);
        ctx.lineTo(np.x, np.y);
    });
    ctx.closePath();
    ctx.stroke();
    if (selected) {
        ctx.fillStyle = 'red';
        ctx.fill();
    }
}

function drawFacets(ctx, facets, normalizer) {
    var selected = {};
    $('.facet-button.selected').each(function (_, e) {
        console.log(selected);
        selected[parseInt($(e).data('facet-index'))] = true;
    });
    for (var i = 0; i < facets.length; ++i) {
        drawFacet(ctx, facets[i], selected[i], normalizer);
    }
}

function drawArrow(ctx, segment, normalizer) {
    ctx.strokeStyle = 'green';
    ctx.lineWidth = 5;
    ctx.setLineDash([4, 4]);

    var p1 = normalizer(segment[0]);
    var p2 = normalizer(segment[1]);
    var angle = Math.atan2(p2.y - p1.y, p2.x - p1.x);
    ctx.beginPath();
    ctx.moveTo(p1.x, p1.y);
    ctx.lineTo(p2.x, p2.y);
    ctx.moveTo(p2.x, p2.y);
    ctx.lineTo(p2.x + 20 * Math.cos(angle + Math.PI * 4 / 5), p2.y + 20 * Math.sin(angle + Math.PI * 4 / 5));
    ctx.moveTo(p2.x, p2.y);
    ctx.lineTo(p2.x + 20 * Math.cos(angle - Math.PI * 4 / 5), p2.y + 20 * Math.sin(angle - Math.PI * 4 / 5));
    ctx.stroke();
}

function drawSkeletons(ctx, skeletons, normalizer) {
    var selected = $('.skeleton-button.selected').data('skeleton-index');
    skeletons.forEach(function (skeleton, i) {
        ctx.beginPath();
        var p1 = normalizer(skeleton[0]);
        ctx.moveTo(p1.x, p1.y);
        var p2 = normalizer(skeleton[1]);
        ctx.lineTo(p2.x, p2.y);
        ctx.setLineDash([4, 4]);
        if (i == selected) {
            ctx.strokeStyle = 'green';
            ctx.lineWidth = 5;

            var angle = Math.atan2(p2.y - p1.y, p2.x - p1.x);
            ctx.moveTo(p2.x, p2.y);
            ctx.lineTo(p2.x + 20 * Math.cos(angle + Math.PI * 4 / 5), p2.y + 20 * Math.sin(angle + Math.PI * 4 / 5));
            ctx.moveTo(p2.x, p2.y);
            ctx.lineTo(p2.x + 20 * Math.cos(angle - Math.PI * 4 / 5), p2.y + 20 * Math.sin(angle - Math.PI * 4 / 5));
        } else {
            ctx.strokeStyle = 'black';
            ctx.lineWidth = 1;
        }
        ctx.stroke();
        ctx.setLineDash([]);

        if ($('#show-label').prop('checked')) {
            ctx.font = '14px Arial';
            ctx.strokeStyle = 'black';
            ctx.lineWidth = 1;
            ctx.strokeText(i, (p1.x + p2.x) / 2, (p1.y + p2.y) / 2);
        }
    });
}

function parseFraction(str) {
    var parts = str.split('/');
    var realValue = parseFloat(parts[0]);
    if (parts.length == 2) {
        realValue = parseFloat(parts[0]) / parseFloat(parts[1]);
    }
    return realValue;
}

function parseCoord(str) {
    var parts = str.split(',');
    var x = parseFraction(parts[0]);
    var y = parseFraction(parts[1]);
    return {
        x: x,
        y: y,
        str: str
    };
}

function getDirectSegment() {
    var start = parseCoord($('#start-coord').val());
    var end = parseCoord($('#end-coord').val());
    return [start, end];
}

function drawThings() {
    var canvas = document.getElementById('canvas');
    var ctx = canvas.getContext('2d');
    ctx.clearRect(0, 0, 1000, 1000);
    var normalizer = generateNormalizer(window.state, canvas.height);
    drawFacets(ctx, window.state.facets, normalizer);
    drawSkeletons(ctx, window.state.skeletons, normalizer);
    if ($('#direct-segment').prop('checked')) {
        drawArrow(ctx, getDirectSegment(), normalizer);
    }
}

function generateNormalizer(state, height) {
    var minX = 1.0 / 0.0;
    var minY = 1.0 / 0.0;
    state.facets.forEach(function (f) {
        f.forEach(function (p) {
            minX = Math.min(minX, p.x);
            minY = Math.min(minY, p.y);
        });
    });
    state.skeletons.forEach(function (s) {
        minX = Math.min(minX, s[0].x);
        minX = Math.min(minX, s[1].x);
        minY = Math.min(minY, s[0].y);
        minY = Math.min(minY, s[1].y);
    });

    var SCALE_FACTOR = window.scaleFactor;
    var offset = 30;
    return function (p) {
        return {
            x: (p.x - minX) * SCALE_FACTOR + offset,
            y: height - ((p.y - minY) * SCALE_FACTOR) - offset
        };
    };
}

function updateSkeletonList(skeletons) {
    var skeletonListDiv = $('#skeleton-list');
    skeletonListDiv.html('');
    for (var i = 0; i < skeletons.length; ++i) {
        skeletonListDiv.append('<button class="btn btn-default skeleton-button" id="skeleton-button' + i + '" data-skeleton-index="' + i + '">S' + i + '</button>');
    }
    $('.skeleton-button').click(function () {
        $('.skeleton-button').removeClass('selected');
        $(this).addClass('selected');
        drawThings();
    });
}

function updateFacetList(facets) {
    var facetListDiv = $('#facet-list');
    facetListDiv.html('');
    facets.forEach(function (facet, i) {
        facetListDiv.append('<button class="btn btn-default facet-button" id="facet-button' + i + '" data-facet-index="' + i + '">F' + i + '</button>');
    });
    $('.facet-button').click(function () {
        var self = $(this);
        self.toggleClass('selected');
        drawThings();
    });
}

function updateState(state) {
    if (state.phase == 'end') {
        $('#solution-panel').show();
        $('#solution').val(state.solution);
        $('#size-indicator').text('Size: ' + state.solution.replace(/\s/g, '').length);
    } else {
        window.state = state;
        $('#history').val(state.history);
        $('#undo-history').val(state.undoHistory);
        $('.facet-button').removeClass('selected');
        $('.skeleton-button').removeClass('selected');
        drawThings();
        updateFacetList(state.facets);
        updateSkeletonList(state.skeletons);
    }

    if (state.phase == 'init') {
        log('基準にする辺を選んでね');
    }
}

$(document).ready(function () {
    window.scaleFactor = 300;
    $('#solution-panel').hide();
    $('#start').click(function () {
        var problemId = $('#problem-id').val();
        $.ajax({
            url: '/gekirin/start',
            data: {
                problemId: problemId
            },
            dataType: 'json',
            method: 'POST'
        }).done(function (data) {
            updateState(data.state);
        }).fail(function (data) {
            $('#information').html('Error: ' + data.errorMessage);
            $('#information').addClass('alert alert-danger');
        });
    });
    $('.submit-button').click(function () {
        var self = $(this);
        var phase = self.attr('id') == 'finish' ? 'end' : window.state.phase;
        var facetIds = $.map($('.facet-button.selected'), function (elem) {
            return $(elem).data('facet-index');
        });
        var skeletonId = $('.skeleton-button.selected').data('skeleton-index');
        var skeleton = [];
        if (phase == 'init' || phase == 'fold') {
            if ($('#direct-segment').prop('checked')) {
                var segment = getDirectSegment();
                skeleton = [
                    segment[0].str,
                    segment[1].str
                ];
            } else {
                skeleton = [
                    window.state.skeletons[skeletonId][0].str,
                    window.state.skeletons[skeletonId][1].str
                ];
            }
        }
        var foldDirection = $('input[name="direction"]:checked').val();
        var history = $('#history').val();
        var problemId = $('#problem-id').val();
        $.ajax({
            url: '/gekirin/send',
            data: {
                facetIds: facetIds,
                skeleton: skeleton,
                foldDirection: foldDirection,
                history: history,
                phase: phase,
                problemId: problemId
            },
            dataType: 'json',
            method: 'POST'
        }).done(function (data) {
            if (data.errorMessage != null) {
                $('#information').html('Error: ' + data.errorMessage);
                $('#information').addClass('alert alert-danger');
            } else {
                updateState(data.state);
            }
        });
    });
    $('#submit').click(function () {
        var self = $(this);
        var solution = $('#solution').val();
        var problemId = $('#problem-id').val();
        $.ajax({
            url: '/gekirin/submit',
            data: {
                problemId: problemId,
                solution: solution
            },
            dataType: 'json',
            method: 'POST'
        }).done(function (data) {
            clearLog();
            log('提出したよ！');
            var times = 0;
            var timer = setInterval(function() {
                $.ajax({
                    url: '/gekirin/submission_status',
                    data: {
                        problemId: problemId,
                        executableName: 'ame-no-gekirin'
                    },
                    method: 'GET'
                }).done(function(data) {
                    if (data.status == 'rejected_redundant') {
                        error('もう満点とってるので弾かれました');
                        clearInterval(timer);
                    } else if(data.status == 'complete') {
                        log('提出成功しました。点数: ' + data.resemblance);
                        clearInterval(timer);
                    } else if (data.status == 'missing') {
                        error('提出失敗したっぽいです（でかすぎ？）');
                        clearInterval(timer);
                    }
                });
                times++;
                if (times == 10) {
                    clearInterval(timer);
                }
            }, 1000);
        });
    });
    $('#select-all-facet').click(function () {
        var self = $(this);
        $('.facet-button').addClass('selected');
        drawThings();
    });
    $('#select-odd').click(function() {
        $('.facet-button').each(function(_, e) {
            if ($(e).data('facet-index') % 2 == 1) {
                $(e).addClass('selected');
            }
        });
        drawThings();
    });
    $('#select-even').click(function() {
        $('.facet-button').each(function(_, e) {
            if ($(e).data('facet-index') % 2 == 0) {
                $(e).addClass('selected');
            }
        });
        drawThings();
    });
    $('#problem-id').keydown(function (e) {
        if (e.keyCode == 13) {
            $('#start').click();
        }
    });
    $('#restore').click(function () {
        var problemId = $('#problem-id').val();
        var history = $('#history').val();
        $.ajax({
            url: '/gekirin/restore',
            data: {
                problemId: problemId,
                history: history
            },
            dataType: 'json',
            method: 'POST'
        }).done(function (data) {
            clearLog();
            if (data.success) {
                log('よくぞ もどってきた！');
                updateState(data.state);
            } else {
                error('じゅもんが ちがいます');
            }
        })
    });
    $('#direct-segment').click(function () {
        var self = $(this);
        if (self.prop('checked')) {
            $('#skeleton-list').hide();
            $('#direct-segment-form').show();
        } else {
            $('#skeleton-list').show();
            $('#direct-segment-form').hide();
        }
    });
    $('#direct-segment-form').hide();
    $('#direct-segment-draw').click(function () {
        drawThings();
    });
    $('#direct-segment').prop('checked', false);
    $('#show-label').click(function() {
        drawThings();
    });
    $('#undo').click(function() {
        $('#history').val($('#undo-history').val());
        $('#restore').click();
    });
    $('.zoom-button').click(function() {
        window.scaleFactor += parseInt($(this).data('zoom-level'));
        drawThings();
    });
    var problemIdMatch = window.location.href.match(/problemId=(\d+)/);
    if (problemIdMatch !== null) {
        $('#problem-id').val(problemIdMatch[1]);
        $('#start').click();
    }
});
