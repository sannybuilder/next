import { computed, Injectable, signal } from '@angular/core';
import compilerInit, {
  compile_binary,
  compile_text_with_definitions,
  get_ssc_version,
} from '../../../somersault-wasm/pkg';
@Injectable({
  providedIn: 'root',
})
export class EditorService {
  initialSource = signal<string[]>([]); // to force signal update on change
  source = signal('');
  definitions = signal('');
  sscVersion = signal('');
  isReady = signal(false);

  constructor() {
    Promise.all([compilerInit(), this.loadJson()]).then(([_, definitions]) => {
      this.isReady.set(true);
      this.definitions.set(definitions);
      this.sscVersion.set(get_ssc_version());
    });
  }

  async loadJson() {
    return fetch('https://raw.githubusercontent.com/sannybuilder/library/refs/heads/master/sa/sa.json').then((x) => x.text());
  }

  transpiled = computed(() => {
    if (this.isReady()) {
      const source = this.source();
      const definitions = this.definitions();
      try {
        return compile_text_with_definitions(source, definitions);
      } catch (e) {
        throw e;
      }
    }
    return '';
  });

  compiled = computed(() => {
    if (this.isReady()) {
      const source = this.source();
      try {
        return compile_binary(source);
      } catch (e) {}
    }
    return '';
  });

  messages = computed(() => {
    if (this.isReady()) {
      try {
        const result = this.transpiled();
        return '';
      } catch (e) {
        return e;
      }
    }
    return '';
  });

  setInitialSource(source: string) {
    this.initialSource.set([source]);
    this.source.set(source);
  }

  isDirty() {
    const [initialSource] = this.initialSource();
    return this.source() !== initialSource;
  }
}
