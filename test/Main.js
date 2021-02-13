exports.establishLocalStorageShim = function() {
  const items = {};

  function assertStr(x) {
    if (typeof x !== 'string') {
      throw Error(`[localStorage shim] Expected string value, got '${x}'`);
    }
  }

  // global assignment
  localStorage = {

    setItem(key, val) {
      assertStr(key);
      assertStr(val);

      items[key] = val;
    },

    getItem(key) {
      assertStr(key);

      if (!(key in items))
        return null;

      return items[key];
    },

  }
};
