if (window.ga_code) {

    (function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
        (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();
        a=s.createElement(o), m=s.getElementsByTagName(o)[0];
        a.async=1;
        a.src=g;m.parentNode.insertBefore(a,m)
    })(window, document, 'script', '//www.google-analytics.com/analytics' + (window.ga_code === 'UA-XXXXX-Y' ? '_debug' : '')  + '.js', 'ga');

    ga('create', window.ga_code, 'auto');

    if (window.ga_code === 'UA-XXXXX-Y') {
        ga('set', 'sendHitTask', null);
    }

    $(document).on('shiny:connected', function(e) {
        ga('send', 'pageview');
    });

    $(document).on('shiny:inputchanged', function(e) {
        if (e.name === 'main_tabs' || e.name === 'document_name') {
            ga('send', 'event', 'widget', e.name, e.value);
        }
    });
}
