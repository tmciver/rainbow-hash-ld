(function() {
  var state = {
    files:    [],
    index:    0,
    uploaded: [], // [{ name, url }]
    skipped:  []  // [name]
  };

  var previewObjectUrl = null;

  // ── Page elements ────────────────────────────────────────────────────────────

  var page1 = document.getElementById('page-1');
  var page2 = document.getElementById('page-2');
  var page3 = document.getElementById('page-3');

  document.getElementById('single-file-btn').addEventListener('click', function() {
    document.getElementById('single-file-input').click();
  });
  document.getElementById('dir-file-btn').addEventListener('click', function() {
    document.getElementById('dir-file-input').click();
  });
  document.getElementById('single-file-input').addEventListener('change', function(e) {
    initQueue(Array.from(e.target.files));
  });
  document.getElementById('dir-file-input').addEventListener('change', function(e) {
    initQueue(Array.from(e.target.files));
  });

  document.getElementById('upload-btn').addEventListener('click', doUpload);
  document.getElementById('skip-btn').addEventListener('click', doSkip);
  // ── Queue management ─────────────────────────────────────────────────────────

  function initQueue(files) {
    state.files    = files;
    state.index    = 0;
    state.uploaded = [];
    state.skipped  = [];
    page1.style.display = 'none';
    page2.style.display = '';
    renderCurrentFile();
  }

  function renderCurrentFile() {
    var file = state.files[state.index];

    document.getElementById('file-progress').textContent =
      'File ' + (state.index + 1) + ' of ' + state.files.length;
    document.getElementById('current-filename').textContent = file.name;

    document.getElementById('wizard-title').value       = '';
    document.getElementById('wizard-description').value = '';
    document.getElementById('subject-pills').innerHTML        = '';
    document.getElementById('subject-hidden-inputs').innerHTML = '';
    document.getElementById('subject-concept-input').value = '';
    document.getElementById('subject-concept-input').dispatchEvent(new Event('input'));
    setStatus('');

    setButtons(true);
    renderPreview(file);
  }

  // ── File preview ─────────────────────────────────────────────────────────────

  function renderPreview(file) {
    var box = document.getElementById('file-preview');

    if (previewObjectUrl) {
      URL.revokeObjectURL(previewObjectUrl);
      previewObjectUrl = null;
    }
    box.innerHTML = '';

    var type = file.type || '';

    if (type.startsWith('image/')) {
      previewObjectUrl = URL.createObjectURL(file);
      var img = document.createElement('img');
      img.src = previewObjectUrl;
      box.appendChild(img);

    } else if (type === 'application/pdf') {
      previewObjectUrl = URL.createObjectURL(file);
      var frame = document.createElement('iframe');
      frame.src = previewObjectUrl;
      box.appendChild(frame);

    } else if (type.startsWith('text/')) {
      var reader = new FileReader();
      reader.onload = function(ev) {
        var pre = document.createElement('pre');
        pre.textContent = ev.target.result.slice(0, 4096);
        box.appendChild(pre);
      };
      reader.readAsText(file.slice(0, 4096));

    } else {
      var label = document.createElement('span');
      label.className = 'text-muted';
      label.textContent = file.name;
      box.appendChild(label);
    }
  }

  // ── Upload ───────────────────────────────────────────────────────────────────

  function doUpload() {
    setButtons(false);
    setStatus('Uploading…', 'text-muted');

    var file  = state.files[state.index];
    var form  = new FormData();
    form.append('file', file, file.name);

    var title = document.getElementById('wizard-title').value.trim();
    var desc  = document.getElementById('wizard-description').value.trim();
    if (title) form.append('title', title);
    if (desc)  form.append('description', desc);

    var hiddenInputs = document.getElementById('subject-hidden-inputs').querySelectorAll('input');
    hiddenInputs.forEach(function(input) {
      form.append(input.name, input.value);
    });

    fetch('/files', {
      method:  'POST',
      headers: { 'Accept': 'application/json' },
      body:    form
    })
    .then(function(resp) {
      if (resp.status !== 202) {
        throw new Error('Server returned ' + resp.status);
      }
      var jobUrl = resp.headers.get('Location');
      if (!jobUrl) throw new Error('No job location returned');
      pollJob(jobUrl, function(fileUrl) {
        state.uploaded.push({ name: file.name, url: fileUrl });
        setStatus('Uploaded', 'text-success');
        setTimeout(advanceToNext, 600);
      }, function(msg) {
        setStatus('Error: ' + msg, 'text-danger');
        setButtons(true);
      });
    })
    .catch(function(err) {
      setStatus('Error: ' + err.message, 'text-danger');
      setButtons(true);
    });
  }

  function doSkip() {
    state.skipped.push(state.files[state.index].name);
    advanceToNext();
  }

  function advanceToNext() {
    state.index++;
    if (state.index < state.files.length) {
      renderCurrentFile();
    } else {
      showSummary();
    }
  }

  // ── Job polling ──────────────────────────────────────────────────────────────

  function pollJob(jobUrl, onComplete, onError) {
    setTimeout(function() {
      fetch(jobUrl)
        .then(function(resp) {
          if (resp.redirected) {
            onComplete(resp.url);
          } else {
            return resp.json().then(function(data) {
              if (data.status === 'failed') {
                onError(data.message || 'Upload failed');
              } else {
                pollJob(jobUrl, onComplete, onError);
              }
            });
          }
        })
        .catch(function(err) { onError(String(err)); });
    }, 800);
  }

  // ── Summary ──────────────────────────────────────────────────────────────────

  function showSummary() {
    page2.style.display = 'none';
    page3.style.display = '';

    var uploadedList = document.getElementById('uploaded-list');
    uploadedList.innerHTML = '';
    state.uploaded.forEach(function(item) {
      var li = document.createElement('li');
      var a  = document.createElement('a');
      a.href = item.url;
      a.target = '_blank';
      a.textContent = item.name;
      li.appendChild(a);
      uploadedList.appendChild(li);
    });

    var skippedSection = document.getElementById('skipped-section');
    var skippedList    = document.getElementById('skipped-list');
    skippedList.innerHTML = '';
    if (state.skipped.length === 0) {
      skippedSection.style.display = 'none';
    } else {
      skippedSection.style.display = '';
      state.skipped.forEach(function(name) {
        var li = document.createElement('li');
        li.textContent = name;
        skippedList.appendChild(li);
      });
    }
  }

  // ── Helpers ──────────────────────────────────────────────────────────────────

  function setButtons(enabled) {
    document.getElementById('upload-btn').disabled = !enabled;
    document.getElementById('skip-btn').disabled   = !enabled;
  }

  function setStatus(msg, cls) {
    var el = document.getElementById('upload-status');
    el.textContent  = msg;
    el.className    = 'mb-2 small ' + (cls || '');
  }
})();
