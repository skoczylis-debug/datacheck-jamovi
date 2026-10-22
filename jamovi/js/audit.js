'use strict';

// Builds the Step 2 list: for each selected variable, one row per value that
// may need a decision, with a suggested replacement the student can edit.
// Rows: { var, head, value, sug, to }. head = variable name on its first row only.

const NA_STRINGS = ['na', 'n/a', 'n.a.', 'nan', 'null', 'none', 'missing', 'unknown',
    '.', '-', '--', '?', '#n/a', 'not applicable', 'no answer', 'refused', 'dk',
    "don't know", 'dont know'];

const NUMBER_WORDS = { zero: 0, one: 1, two: 2, three: 3, four: 4, five: 5, six: 6,
    seven: 7, eight: 8, nine: 9, ten: 10, eleven: 11, twelve: 12, thirteen: 13,
    fourteen: 14, fifteen: 15, sixteen: 16, seventeen: 17, eighteen: 18,
    nineteen: 19, twenty: 20, thirty: 30, forty: 40, fifty: 50, sixty: 60,
    seventy: 70, eighty: 80, ninety: 90, hundred: 100 };

const MONTHS = { january: 1, february: 2, march: 3, april: 4, may: 5, june: 6, july: 7,
    august: 8, september: 9, october: 10, november: 11, december: 12, jan: 1, feb: 2,
    mar: 3, apr: 4, jun: 6, jul: 7, aug: 8, sep: 9, sept: 9, oct: 10, nov: 11, dec: 12 };

const RANGE_LABEL = 'Valid range (e.g. 1-5)';
const BLANK_LABEL = 'Blank cells mean';
const NEG_CODES = ['-9', '-99', '-999', '-9999', '-8', '-98', '-998', '-7', '-97', '-77', '-88'];
const isImportId = (s) => s.trim().startsWith('{"ImportId"');
const ADMIN_KEYS = ['startdate', 'enddate', 'status', 'ipaddress', 'progress', 'duration',
    'durationinseconds', 'finished', 'recordeddate', 'responseid', 'recipientlastname',
    'recipientfirstname', 'recipientemail', 'externalreference', 'externaldatareference',
    'locationlatitude', 'locationlongitude', 'distributionchannel', 'userlanguage',
    'starttime', 'completiontime', 'lastmodifiedtime', 'email', 'name', 'timestamp',
    'emailaddress', 'startedat', 'completedat', 'submittedat', 'datesubmitted',
    'useragent', 'browser', 'ipaddr', 'completiondate', 'totalpoints', 'quizfeedback'];
const squash = (s) => s.toLowerCase().replace(/[^a-z0-9]/g, '');
const isAdmin = (name) => ADMIN_KEYS.indexOf(squash(name)) !== -1 || /^Q_/.test(name) || /^(Points|Feedback) - /.test(name);

// While extra header rows from a survey export are still in the data, each column also holds its question text.
const looksLikeQuestion = (v, name, numericColumn) => {
    if (squash(v) === squash(name)) return true;
    if (/\?\s*$/.test(v) || v.indexOf(' - ') !== -1) return true;
    return numericColumn && isNaN(parseNum(v)) && v.trim().split(/\s+/).length >= 3;
};
const MAX_LIST = 30;

const fixEncoding = (s) => s.replace(/Â(?=[£ -¿])/g, '');
const tidy = (s) => fixEncoding(s).replace(/\s+/g, ' ').trim();
const keyOf = (s) => tidy(s).toLowerCase();
const parseNum = (s) => {
    let t = tidy(s).replace(/[£$€,\s]/g, '');
    if (t === '' || !/^-?[0-9]*\.?[0-9]+(e-?[0-9]+)?$/i.test(t)) return NaN;
    return Number(t);
};

const guessNumber = (s) => {
    let k = keyOf(s);
    if (NUMBER_WORDS[k] !== undefined) return String(NUMBER_WORDS[k]);
    let m = k.match(/^[£$€]?([0-9.]+)\s*k$/);
    if (m) return String(Number(m[1]) * 1000);
    return '?';
};

// ---- dates
const dateFormat = (t) => {
    if (/^\d{4}-\d{1,2}-\d{1,2}$/.test(t)) return 'iso';
    if (/^\d{1,2}\/\d{1,2}\/\d{4}$/.test(t)) return 'slash';
    if (/^\d{1,2}(st|nd|rd|th)? [A-Za-z]+ \d{4}$/.test(t)) return 'words';
    return null;
};
const dateParts = (t, f) => {
    if (f === 'iso') return t.split('-').map(Number);
    if (f === 'slash') {
        let p = t.split('/').map(Number);
        if (p[1] > 12 && p[0] <= 12) return [p[2], p[0], p[1]];     // US order
        return [p[2], p[1], p[0]];                                   // UK order
    }
    let w = t.split(' ');
    let m = MONTHS[w[1].toLowerCase()];
    if (m === undefined) return null;
    return [Number(w[2]), m, Number(w[0].replace(/\D/g, ''))];
};
const validDate = (p) => {
    if (!p) return false;
    let d = new Date(Date.UTC(p[0], p[1] - 1, p[2]));
    return d.getUTCFullYear() === p[0] && d.getUTCMonth() === p[1] - 1 && d.getUTCDate() === p[2];
};
const pad = (n) => (n < 10 ? '0' : '') + n;
const writeDate = (p, f) => (f === 'slash' ? pad(p[2]) + '/' + pad(p[1]) + '/' + p[0] : p[0] + '-' + pad(p[1]) + '-' + pad(p[2]));

// ---- the readable spelling of a group of variants: no stray spaces, Title case if possible
const pickDisplay = (variants) => {
    let clean = variants.filter(v => v === v.trim() && !/\s{2,}/.test(v));
    let pool = clean.length > 0 ? clean : variants.map(tidy);
    let title = pool.filter(v => /^[A-Z][a-z]/.test(v));
    let caps = pool.filter(v => /^[A-Z]/.test(v));
    return (title.length > 0 ? title : caps.length > 0 ? caps : pool)[0];
};

const initials = (k) => k.split(' ').map(w => w.charAt(0)).join('');

// every variable (except admin columns) ends with a "Blank cells mean" row
const rowsFor = (name, info) => {
    if (isAdmin(name)) return [];
    let rows = rowsCore(name, info);
    if (!rows.some(r => r.value === BLANK_LABEL))
        rows.push({ value: BLANK_LABEL, sug: 'missing', to: 'missing' });
    return rows;
};

// rows for one variable from its column info
const rowsCore = (name, info) => {
    let rows = [];
    let numericType = info.dataType === 'integer' || info.dataType === 'decimal';
    let allLevels = (info.levels || []).filter(l => !l.treatAsMissing).map(l => String(l.importValue !== undefined ? l.importValue : l.label));
    let hasHeader = allLevels.some(isImportId);          // survey header rows still in the data
    let levels = allLevels.filter(v => !isImportId(v));
    if (hasHeader) {
        let numShare = levels.filter(v => !isNaN(parseNum(v))).length / Math.max(1, levels.length);
        levels = levels.filter(v => !looksLikeQuestion(v, name, numShare >= 0.5));
    }

    if (isAdmin(name))
        return rows;                 // survey admin columns: flagged in the report, nothing to decide

    if (numericType) {
        rows.push({ value: RANGE_LABEL, sug: '', to: '' });
        return rows;
    }

    // group spelling variants (capitals, spaces)
    let groups = {};
    let order = [];
    for (let v of levels) {
        let k = keyOf(v);
        if (groups[k] === undefined) { groups[k] = []; order.push(k); }
        groups[k].push(v);
    }
    let na = order.filter(k => k === '' || NA_STRINGS.indexOf(k) !== -1);
    let real = order.filter(k => na.indexOf(k) === -1);
    // in text columns, negative codes such as -99 mean "seen, not answered"
    let words = real.filter(k => isNaN(parseNum(k)));
    if (real.length > 0 && words.length / real.length >= 0.6) {
        let neg = real.filter(k => NEG_CODES.indexOf(k) !== -1);
        na = na.concat(neg);
        real = real.filter(k => neg.indexOf(k) === -1);
    }
    if (real.length === 0 && na.length === 0) return rows;

    // open-ended answers: nothing to decide value by value
    let avgLen = real.reduce((a, k) => a + k.length, 0) / Math.max(1, real.length);
    if (/_(COMMENT|TEXT|OTHER)$/i.test(name) || (real.length >= 5 && avgLen > 30))
        return rows;

    const display = (k) => (k === '' ? '(spaces only)' : pickDisplay(groups[k]));
    let naRows = na.map(k => ({ value: display(k), sug: 'missing', to: 'missing' }));

    // tick-box column ("select all that apply"): one answer, the rest blank
    if (real.length === 1 && (/_[0-9]+$/.test(name) || !hasHeader)) {
        rows.push({ value: BLANK_LABEL, sug: 'not ticked (0)', to: 'not ticked (0)' });
        return rows.concat(naRows);
    }

    // numbers stored as text
    let nums = real.filter(k => !isNaN(parseNum(k)));
    if (real.length > 0 && nums.length / real.length >= 0.6) {
        for (let k of real) {
            if (!isNaN(parseNum(k))) continue;
            let s = guessNumber(k);
            rows.push({ value: display(k), sug: s, to: s });
        }
        rows = rows.concat(naRows);
        rows.push({ value: RANGE_LABEL, sug: '', to: '' });
        return rows;
    }

    // dates
    let fmts = {};
    let dated = 0;
    for (let k of real) { let f = dateFormat(tidy(groups[k][0])); fmts[k] = f; if (f) dated++; }
    if (real.length > 0 && dated / real.length >= 0.6) {
        let counts = {};
        for (let k of real) if (fmts[k]) counts[fmts[k]] = (counts[fmts[k]] || 0) + 1;
        let dom = Object.keys(counts).sort((a, b) => counts[b] - counts[a])[0];
        for (let k of real) {
            let t = tidy(groups[k][0]);
            let p = fmts[k] ? dateParts(t, fmts[k]) : null;
            if (!validDate(p)) rows.push({ value: t, sug: '?', to: '?' });
            else if (fmts[k] !== dom) {
                let s = writeDate(p, dom === 'slash' ? 'slash' : 'iso');
                rows.push({ value: t, sug: s, to: s });
            }
        }
        return rows.concat(naRows);
    }

    // categories: suggest a fuller value for abbreviations and cut-off words
    let cands = {};
    for (let g of real) {
        cands[g] = real.filter(h => {
            if (h.length <= g.length) return false;
            if (initials(h) === g && h.indexOf(' ') !== -1) return true;
            if (!h.startsWith(g)) return false;
            if (g.length <= 3) return true;
            return h.indexOf(' ') === -1 && h.length - g.length <= 4;
        });
    }
    const resolve = (g, depth) => {
        if (depth > 10 || cands[g].length === 0) return g;
        let targets = [...new Set(cands[g].map(h => resolve(h, depth + 1)))];
        return targets.length === 1 ? targets[0] : null;
    };
    let wordy = real.filter(k => isNaN(parseNum(k))).length / real.length >= 0.6;

    let list = [];
    for (let g of real) {
        let d = display(g);
        let r = resolve(g, 0);
        let s;
        if (r === null) s = '?';                            // could be more than one thing
        else if (r !== g) s = display(r);
        else if (wordy && !isNaN(parseNum(g))) s = '?';    // a number among words, e.g. 1 in Yes/No
        else s = d;
        list.push({ value: d, sug: s, to: s, changes: s !== d });
    }
    if (list.length > MAX_LIST) list = list.filter(r => r.changes);  // free text: only the odd ones
    const rank = (r) => (r.to === '?' ? 0 : r.changes ? 1 : 2);
    list.sort((a, b) => rank(a) - rank(b) || a.value.localeCompare(b.value));
    return list.map(r => ({ value: r.value, sug: r.sug, to: r.to })).concat(naRows);
};

module.exports = {

    _rowsFor: rowsFor,

    view_loaded: function(ui) {
        this.syncRows(ui);
    },

    view_updated: function(ui) {
        this.syncRows(ui);
    },

    view_remoteDataChanged: function(ui, data) {
        if (data.dataType === 'columns')
            this.syncRows(ui);
    },

    vars_changed: function(ui) {
        this.syncRows(ui);
    },

    id_changed: function(ui) {
        this.syncRows(ui);
    },

    // "Add all other columns": fill the keep box with every other data column
    allCols_changed: function(ui) {
        if (!ui.allCols.value()) {
            ui.keep.setValue([]);
            return;
        }
        let id = ui.id.value();
        let used = (ui.vars.value() || []).concat(id ? [id] : []);
        this.requestData('columns', null).then(rData => {
            let names = (rData.columns || [])
                .filter(c => ['data', 'computed', 'recoded'].indexOf(c.columnType) !== -1)
                .map(c => c.name)
                .filter(n => used.indexOf(n) === -1);
            ui.keep.setValue(names);
        });
    },

    syncRows: function(ui) {
        let id = ui.id.value();
        let vars = (ui.vars.value() || []).filter(v => v !== id);
        this._syncToken = (this._syncToken || 0) + 1;
        let token = this._syncToken;

        let requests = vars.map(v => this.requestData('column', {
            columnName: v, properties: ['dataType', 'measureType', 'levels'] }));

        Promise.all(requests).then(infos => {
            if (token !== this._syncToken)
                return;

            let old = ui.fixes.value() || [];
            let list = [];
            for (let i = 0; i < vars.length; i++) {
                let info = infos[i];
                if (!info || info.columnFound === false) continue;
                let rows = rowsFor(vars[i], info);
                for (let j = 0; j < rows.length; j++) {
                    let r = rows[j];
                    let prev = old.find(o => o.var === vars[i] && o.value === r.value);
                    // keep anything the student typed; refresh untouched suggestions
                    let to = (prev && prev.to !== prev.sug) ? prev.to : r.to;
                    list.push({ var: vars[i], head: j === 0 ? vars[i] : '', value: r.value, sug: r.sug, to: to });
                }
            }
            if (JSON.stringify(old) !== JSON.stringify(list))
                ui.fixes.setValue(list);
        });
    }
};
