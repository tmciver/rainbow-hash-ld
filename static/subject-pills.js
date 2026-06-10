(function() {
  var conceptInput  = document.getElementById('subject-concept-input');
  var addBtn        = document.getElementById('add-subject-btn');
  var pillsDiv      = document.getElementById('subject-pills');
  var hiddenDiv     = document.getElementById('subject-hidden-inputs');
  var suggestionsDiv = document.getElementById('concept-suggestions');

  var pendingConcept = null; // { uri, label } selected from suggestions

  function addSubjectPill(label, uri) {
    var pill = document.createElement('span');
    pill.className = 'badge badge-pill badge-info mr-1 mb-1';
    pill.style.fontSize = '0.9em';
    pill.appendChild(document.createTextNode(label + ' '));

    var hidden = document.createElement('input');
    hidden.type  = 'hidden';
    hidden.name  = 'subject';
    hidden.value = uri;

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
    conceptInput.value = '';
    pendingConcept = null;
    hideSuggestions();
  }

  function addSubject() {
    if (pendingConcept) {
      addSubjectPill(pendingConcept.label, pendingConcept.uri);
    }
  }

  function hideSuggestions() {
    suggestionsDiv.style.display = 'none';
    suggestionsDiv.innerHTML = '';
  }

  function showSuggestions(concepts) {
    suggestionsDiv.innerHTML = '';
    if (concepts.length === 0) {
      hideSuggestions();
      return;
    }
    concepts.forEach(function(concept) {
      var item = document.createElement('button');
      item.type = 'button';
      item.className = 'list-group-item list-group-item-action';
      item.textContent = concept.prefLabel;
      item.addEventListener('mousedown', function(e) {
        e.preventDefault();
        pendingConcept = { label: concept.prefLabel, uri: concept.uri };
        conceptInput.value = concept.prefLabel;
        hideSuggestions();
        addSubjectPill(concept.prefLabel, concept.uri);
      });
      suggestionsDiv.appendChild(item);
    });
    suggestionsDiv.style.display = 'block';
  }

  var debounceTimer = null;
  conceptInput.addEventListener('input', function() {
    clearTimeout(debounceTimer);
    pendingConcept = null;
    var q = conceptInput.value.trim();
    if (!q) {
      hideSuggestions();
      return;
    }
    debounceTimer = setTimeout(function() {
      fetch('/concepts?q=' + encodeURIComponent(q))
        .then(function(r) { return r.json(); })
        .then(showSuggestions)
        .catch(function() { hideSuggestions(); });
    }, 200);
  });

  conceptInput.addEventListener('blur', function() {
    setTimeout(hideSuggestions, 150);
  });

  addBtn.addEventListener('click', addSubject);
  conceptInput.addEventListener('keydown', function(e) {
    if (e.key === 'Enter') {
      e.preventDefault();
      addSubject();
    }
  });
})();
