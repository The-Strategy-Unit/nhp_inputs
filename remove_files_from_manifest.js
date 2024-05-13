const fs = require("fs");
const m = JSON.parse(fs.readFileSync("manifest.json", { "encoding": "utf-8" }));
m["files"] = {}

fs.writeFileSync("manifest.json", JSON.stringify(m, null, 2));