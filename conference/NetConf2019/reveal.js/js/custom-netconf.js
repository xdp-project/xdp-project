var logoWhite = '<img src="./reveal.js/images/Logo-RedHat-A-White-RGB.svg" />';
var logoHat = '<img src="./reveal.js/images/Logo-RedHat-Hat-White-RGB.svg" />';
var logoRed = '<img src="./reveal.js/images/Logo-RedHat-A-Reverse-RGB.svg" />';

function getAuthorHTML(title, authors) {
    return [
        '<span class="title">',
        title,
        '</span>',
        '<span class="authors">',
        processAuthors(authors),
        '</span>',
    ].join('');
}

function processAuthors(authors) {
    authors = authors.replace(/^\(|\)$/g, "");
    authors = authors.replace(/</g, "&lt;");
    authors = authors.replace(/>/g, "&gt;");
    authors = authors.replace(/[a-z0-9\._-]+@[a-z0-9_-]+(\.[a-z0-9_-]+)+/g,
                              function(x) {
                                  return "<a href=\"mailto:" + x + "\">"+x+"</a>";
                              });
    return authors;
}

function addAuthor(selector, logo) {
    var title = document.querySelectorAll("title")[0].innerText;
    var authors = document.querySelectorAll("meta[name=author]")[0].content;
    var authorHTML = getAuthorHTML(title, authors);
    return function() {
        var elems = document.querySelectorAll(selector);
        elems.forEach(function (e) {
            var author = document.createElement('div');
            author.className = 'authorbox';
            author.innerHTML = logo + authorHTML;
            e.appendChild(author);
        });
    }
}

/* needs to be in an onLoad callback, otherwise hljs is not defined yet */
/*
function addHighlightKeywords() {
        hljs.getLanguage('C').k.keyword += ' XDP_DROP XDP_PASS XDP_ABORTED XDP_REDIRECT XDP_TX';
        hljs.getLanguage('C').k.keyword += ' data data_end';
        hljs.getLanguage('C').k.keyword += ' xdp_do_redirect xdp_do_flush_map';
}
window.addEventListener('load', addHighlightKeywords);
*/

window.addEventListener('load', addAuthor('.reveal .slides > section > section', logoRed));
window.addEventListener('load', addAuthor('#sec-title-slide', logoWhite));
