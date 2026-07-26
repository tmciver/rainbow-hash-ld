(function() {
  var conceptId = document.getElementById('concept-detail').dataset.conceptId;

  setupSection('broader');
  setupSection('narrower');

  document.querySelectorAll('.remove-relation-btn').forEach(attachRemoveHandler);

  function setupSection(rel) {
    var searchInput    = document.getElementById(rel + '-search');
    var suggestionsBox = document.getElementById(rel + '-suggestions');
    var addBtn         = document.getElementById('add-' + rel + '-btn');
    var pillsContainer = document.getElementById(rel + '-pills');
    var pending        = null;

    function hideSuggestions() {
      suggestionsBox.style.display = 'none';
      suggestionsBox.innerHTML = '';
    }

    var debounceTimer = null;
    searchInput.addEventListener('input', function() {
      clearTimeout(debounceTimer);
      pending = null;
      addBtn.disabled = true;
      var q = searchInput.value.trim();
      if (!q) { hideSuggestions(); return; }
      debounceTimer = setTimeout(function() {
        fetch('/concepts?q=' + encodeURIComponent(q))
          .then(function(r) { return r.json(); })
          .then(function(concepts) {
            suggestionsBox.innerHTML = '';
            if (!concepts.length) { hideSuggestions(); return; }
            concepts.forEach(function(c) {
              var item = document.createElement('button');
              item.type = 'button';
              item.className = 'list-group-item list-group-item-action';
              item.textContent = c.prefLabel;
              item.addEventListener('mousedown', function(e) {
                e.preventDefault();
                pending = c;
                searchInput.value = c.prefLabel;
                addBtn.disabled = false;
                hideSuggestions();
              });
              suggestionsBox.appendChild(item);
            });
            suggestionsBox.style.display = 'block';
          })
          .catch(function() { hideSuggestions(); });
      }, 200);
    });

    searchInput.addEventListener('blur', function() {
      setTimeout(hideSuggestions, 150);
    });

    addBtn.addEventListener('click', function() {
      if (!pending) return;
      var c = pending;
      addBtn.disabled = true;
      fetch('/concepts/' + conceptId + '/' + rel, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ uri: c.uri })
      })
      .then(function(resp) {
        if (!resp.ok) throw new Error('Server returned ' + resp.status);
        var relatedId = c.uri.split('/').pop();
        pillsContainer.appendChild(makePill(c.prefLabel, c.uri, rel, relatedId));
        pending = null;
        searchInput.value = '';
      })
      .catch(function(err) {
        alert('Failed to add concept: ' + err.message);
        addBtn.disabled = false;
      });
    });
  }

  function makePill(label, uri, rel, relatedId) {
    var span = document.createElement('span');
    span.className = 'badge badge-secondary mr-1 mb-1 concept-pill';

    var link = document.createElement('a');
    link.href = uri;
    link.textContent = label;
    link.className = 'text-white';

    var btn = document.createElement('button');
    btn.type = 'button';
    btn.className = 'concept-pill-remove remove-relation-btn';
    btn.dataset.rel = rel;
    btn.dataset.relatedId = relatedId;
    btn.textContent = '\xd7';
    attachRemoveHandler(btn);

    span.appendChild(link);
    span.appendChild(btn);
    return span;
  }

  function attachRemoveHandler(btn) {
    btn.addEventListener('click', function() {
      var rel       = btn.dataset.rel;
      var relatedId = btn.dataset.relatedId;
      var pill      = btn.closest('.concept-pill');
      fetch('/concepts/' + conceptId + '/' + rel + '/' + relatedId, {
        method: 'DELETE'
      })
      .then(function(resp) {
        if (!resp.ok) throw new Error('Server returned ' + resp.status);
        pill.remove();
      })
      .catch(function(err) {
        alert('Failed to remove concept: ' + err.message);
      });
    });
  }
})();
