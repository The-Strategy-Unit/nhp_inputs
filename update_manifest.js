const fs = require('fs');
const crypto = require('crypto');

// update this list with any files that should be included in manifest
const files = [
  "DESCRIPTION",
  "NAMESPACE",
  "app.R",
  fs.readdirSync("R", { recursive: true }).map(f => "R/" + f),
  fs.readdirSync("inst", { recursive: true }).map(f => "inst/" + f)
]
  .flat()
  // keep files, ignore directories
  .filter(f => !fs.lstatSync(f).isDirectory())
  // convert windows file paths to unix file paths
  .map(f => f.replace(/\\/g, "/"));

// returns the md5 checksum of a given file
function hashfile(file_path) {
  const hash = crypto.createHash("md5");
  const file = fs.readFileSync(file_path);

  hash.update(file);
  return { "checksum": hash.digest("hex") };
}

// load the current manifest.json file
const manifest = require("./manifest.json");
// replace the entire list of files inside manifest
manifest["files"] = Object.assign({}, ...files.map(f => ({ [f]: hashfile(f) })));
// write the updated manifest to disk
fs.writeFileSync("manifest.json", JSON.stringify(manifest, null, 2));
