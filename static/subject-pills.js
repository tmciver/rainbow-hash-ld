(function() {
  var urlInput  = document.getElementById('subject-url-input');
  var addBtn    = document.getElementById('add-subject-btn');
  var pillsDiv  = document.getElementById('subject-pills');
  var hiddenDiv = document.getElementById('subject-hidden-inputs');

  function addSubject() {
    var url = urlInput.value.trim();
    if (!url) return;

    var pill = document.createElement('span');
    pill.className = 'badge badge-pill badge-info mr-1 mb-1';
    pill.style.fontSize = '0.9em';
    pill.appendChild(document.createTextNode(url + ' '));

    var hidden = document.createElement('input');
    hidden.type  = 'hidden';
    hidden.name  = 'subject';
    hidden.value = url;

    var remove = document.createElement('a');
    remove.href = '#';
    remove.textContent = '\u00d7';
    remove.style.color = 'white';
    remove.addEventListener('click', function(e) {
      e.preventDefault();
      pill.remove();
      hidden.remove();
    });

    pill.appendChild(remove);
    pillsDiv.appendChild(pill);
    hiddenDiv.appendChild(hidden);
    urlInput.value = '';
  }

  addBtn.addEventListener('click', addSubject);
  urlInput.addEventListener('keydown', function(e) {
    if (e.key === 'Enter') {
      e.preventDefault();
      addSubject();
    }
  });
})();
