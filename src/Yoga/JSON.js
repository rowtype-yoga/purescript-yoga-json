function reviver(key, value) {
  if (key === "big") {
    return BigInt(value);
  }
  return value;
}
export const _parseJSON = (payload) => JSON.parse(payload, reviver);

export const _undefined = undefined;

function replacer(key, value) {
  if (typeof value === "bigint") {
    return value.toString();
  }
  return value;
}

export const _unsafeStringify = (data) => JSON.stringify(data, replacer);
export const _unsafePrettyStringify = spaces => data => JSON.stringify(data, replacer, spaces);
