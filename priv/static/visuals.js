
//Creates a graph for sentiment analysis using chart.js
function createSentimentGraph(good, bad, equal, total, type) {

    var context = document.getElementById("graphcanvas").getContext("2d");
    if (window.sentimentChart) {
        window.sentimentChart.destroy();
    }

    window.sentimentChart = new Chart(context, {
        type: type,

        options: {
            maintainAspectRatio: false,
            responsive: true,
            scales: {

                yAxes: [{
                        ticks: {
                            beginAtZero:true
                        }
                }]
            }
        },

        data: {
            labels: [],
            datasets: [{
                label: "Good",
                fill: false,
                lineTension: 0.1,
                backgroundColor: "rgb(0,102,51)",
                borderColor: "rgb(0,102,51)",                
                borderDash: [],
                borderDashOffset: 0.0,
                borderJoinStyle: 'miter',
                pointBorderColor: "rgb(0,102,51)",
                pointBackgroundColor: "#fff",
                pointBorderWidth: 1,
                pointHoverRadius: 5,
                pointHoverBackgroundColor: "rgb(0,102,51)",
                pointHoverBorderColor: "rgb(0,102,51)",
                pointHoverBorderWidth: 2,
                pointRadius: 1,
                pointHitRadius: 10,
                data: good
            },
            {
                label: "Bad",
                fill: false,
                lineTension: 0.1,
                backgroundColor: "rgb(204,0,0)",
                borderColor: "rgb(204,0,0)",                
                borderDash: [],
                borderDashOffset: 0.0,
                borderJoinStyle: 'miter',
                pointBorderColor: "rgb(204,0,0)",
                pointBackgroundColor: "#fff",
                pointBorderWidth: 1,
                pointHoverRadius: 5,
                pointHoverBackgroundColor: "rgb(204,0,0)",
                pointHoverBorderColor: "rgb(204,0,0)",
                pointHoverBorderWidth: 2,
                pointRadius: 1,
                pointHitRadius: 10,
                data: bad
            },
            {
                label: "Equal",
                fill: false,
                lineTension: 0.1,
                backgroundColor: "rgb(255,204,0)",
                borderColor: "rgb(255,204,0)",                
                borderDash: [],
                borderDashOffset: 0.0,
                borderJoinStyle: 'miter',
                pointBorderColor: "rgb(255,204,0)",
                pointBackgroundColor: "#fff",
                pointBorderWidth: 1,
                pointHoverRadius: 5,
                pointHoverBackgroundColor: "rgb(255,204,0)",
                pointHoverBorderColor: "rgb(255,204,0)",
                pointHoverBorderWidth: 2,
                pointRadius: 1,
                pointHitRadius: 10,
                data: equal
            },
            {
                label: "Total",
                fill: false,
                lineTension: 0.1,
                backgroundColor: "rgb(0,0,0)",
                borderColor: "rgb(0,0,0)",                
                borderDash: [],
                borderDashOffset: 0.0,
                borderJoinStyle: 'miter',
                pointBorderColor: "rgb(0,0,0)",
                pointBackgroundColor: "#fff",
                pointBorderWidth: 1,
                pointHoverRadius: 5,
                pointHoverBackgroundColor: "rgb(0,0,0)",
                pointHoverBorderColor: "rgb(0,0,0)",
                pointHoverBorderWidth: 2,
                pointRadius: 1,
                pointHitRadius: 10,
                data: total
            }]
        }
    })
}
//Creates a graph for common words using chart.js
function createWordGraph(labels, data) {

    var context = document.getElementById("wordcanvas").getContext("2d");
    if (window.wordChart) {
        window.wordChart.destroy();
    }

    window.wordChart = new Chart(context, {
        type: 'bar',

        options: {
            maintainAspectRatio: false,
            responsive: true
        },

        data: {
            labels: labels,
            datasets: [{
                label: "Word count",
                fill: false,
                lineTension: 0.1,
                backgroundColor: "rgb(0,102,51)",
                borderColor: "rgb(0,102,51)",                
                borderDash: [],
                borderDashOffset: 0.0,
                borderJoinStyle: 'miter',
                pointBorderColor: "rgb(0,102,51)",
                pointBackgroundColor: "#fff",
                pointBorderWidth: 1,
                pointHoverRadius: 5,
                pointHoverBackgroundColor: "rgb(0,102,51)",
                pointHoverBorderColor: "rgb(0,102,51)",
                pointHoverBorderWidth: 2,
                pointRadius: 1,
                pointHitRadius: 10,
                data: data
            }]
        }
    })
}