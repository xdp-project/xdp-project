var titleFooter = 'XDP as a building block for other FOSS projects'
var authorName = 'Jesper Dangaard Brouer';
var authorEmail = 'brouer@redhat.com';

var authorHTML = [
    '<img src="./reveal.js/images/RedHat.svg" />',
    '<span class="footer">',
    '&nbsp;&nbsp;&nbsp;',
    '&nbsp;&nbsp;&nbsp;',
    '&nbsp;&nbsp;&nbsp;',
    '&nbsp;&nbsp;&nbsp;',
    '&nbsp;&nbsp;&nbsp;',
    '&nbsp;&nbsp;&nbsp;',
    titleFooter,
    '</span>',
    '<span class="authors">&nbsp;&nbsp; - &nbsp;&nbsp;',
    authorName, ' &lt;', authorEmail, '&gt; &nbsp;</span>',
].join('');

function addAuthor(selector) {
    return function() {
        var elems = document.querySelectorAll(selector);
        elems.forEach(function (e) {
            var author = document.createElement('div');
            author.className = 'authorbox';
            author.innerHTML = authorHTML;
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

window.addEventListener('load', addAuthor('.reveal .slides > section > section'));
window.addEventListener('load', addAuthor('#sec-title-slide'));
