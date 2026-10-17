function annotateNrPayloads(codingScheme, batches) {
  const object = value => value !== null && typeof value === 'object' && !Array.isArray(value);
  const string = value => typeof value === 'string' ? value : null;
  const contains = (values, value) => Array.isArray(values) ? values.includes(value) : values === value;
  let scheme;
  try { scheme = JSON.parse(codingScheme); } catch (_) { scheme = null; }
  const variables = object(scheme) && Array.isArray(scheme.variableCodings) &&
    scheme.variableCodings.every(object) ? scheme.variableCodings : [];
  const ids = variables.map(variable => string(variable.id));
  const aliases = variables.map(variable => string(variable.alias));
  const sourceTypes = variables.map(variable => string(variable.sourceType));
  const transforms = variables.map(variable => object(variable.sourceParameters) &&
    contains(variable.sourceParameters.processing, 'TAKE_NOT_REACHED_AS_VALUE_CHANGED'));

  const add = (map, key, position) => {
    if (key !== null) {
      if (!map.has(key)) map.set(key, []);
      map.get(key).push(position);
    }
  };
  const byId = new Map();
  const bases = new Map();
  ids.forEach((id, position) => {
    add(byId, id, position);
    if (sourceTypes[position] === 'BASE') add(bases, id, position);
  });
  const byAlias = new Map();
  aliases.forEach((alias, position) => {
    if (alias === null || alias === '') return;
    // A derived alias reusing its own BASE source's ID does not shadow the
    // input needed to derive that result. Match the coder's resolution rule.
    if (sourceTypes[position] !== null &&
        !['BASE', 'BASE_NO_VALUE'].includes(sourceTypes[position]) &&
        bases.has(alias) && bases.get(alias).length === 1 &&
        contains(variables[position].deriveSources, alias)) return;
    add(byAlias, alias, position);
  });
  const resolve = rawId => {
    if (typeof rawId !== 'string') return null;
    const candidates = byAlias.has(rawId) ? byAlias.get(rawId) : byId.get(rawId);
    if (!candidates || candidates.length !== 1) return null;
    const position = candidates[0];
    return ids[position] !== null && byId.get(ids[position]).length === 1 ? position : null;
  };

  const annotate = (payload, inheritStatus) => {
    let records;
    try { records = JSON.parse(payload); } catch (_) { return payload; }
    if (!Array.isArray(records) || !records.every(object)) return payload;
    const positions = records.map(record => resolve(record.id));
    const keys = records.map((record, index) =>
      JSON.stringify([positions[index], string(record.subform) || '']));
    const counts = new Map();
    keys.forEach(key => counts.set(key, (counts.get(key) || 0) + 1));
    records.forEach((record, index) => {
      delete record.coding_from_not_reached;
      // Overlays without a status inherit the original record's status and
      // marker. An explicit null instead overwrites them with unknown origin.
      if (inheritStatus && !Object.prototype.hasOwnProperty.call(record, 'status')) return;
      const position = positions[index];
      let flag = null;
      if (position !== null && counts.get(keys[index]) === 1 && sourceTypes[position] !== null) {
        if (sourceTypes[position] !== 'BASE' || !transforms[position]) {
          flag = false;
        } else if (typeof record.status === 'string') {
          flag = record.status === 'NOT_REACHED';
        }
      }
      record.coding_from_not_reached = flag;
    });
    return JSON.stringify(records);
  };
  const output = {};
  Object.keys(batches).forEach(column => {
    const batch = batches[column];
    output[column] = batch.values.map(payload => annotate(payload, batch.inheritStatus));
  });
  return output;
}
