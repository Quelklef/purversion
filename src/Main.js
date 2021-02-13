exports.nativeSet = key => item => () => localStorage.setItem(key, item);
exports.nativeGet = key => () => localStorage.getItem(key) || "";
exports.exists = key => () => localStorage.getItem(key) !== null;
