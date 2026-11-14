import { readFileSync } from "node:fs";
import { render } from "./pele.mjs";

const [input, fontFamily] = process.argv.slice(2);

try {
  const out = render(readFileSync(input, "utf8"), { fontFamily });
  process.stdout.write(
    JSON.stringify({ svg: out.svg, width: out.width, height: out.height }),
  );
} catch (err) {
  process.stderr.write(String(err && err.message ? err.message : err));
  process.exit(1);
}
