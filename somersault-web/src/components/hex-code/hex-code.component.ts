import { Component, computed } from '@angular/core';
import { EditorService } from '../../state/editor.service';

@Component({
  selector: 'app-hex-code',
  imports: [],
  templateUrl: './hex-code.component.html',
  styleUrl: './hex-code.component.scss',
})
export class HexCodeComponent {
  hexed = computed(() => {
    const data = this.editor.compiled();

    return (data ?? '')
      .trim()
      .split(' ')
      .map((x) => {
        if (x === '00') {
          return `<span class='zero'>${x}</span>`;
        }
        return `<span>${x}</span>`;
      })
      .join('');
  });

  printable = computed(() => {
    const data = this.editor.compiled();

    return (data ?? '')
      .trim()
      .split(' ')
      .map((x) => {
        const charCode = parseInt(x, 16);

        if (charCode >= 32 && charCode < 127) {
          return `<span>${String.fromCharCode(charCode)}</span>`;
        }
        return `<span>·</span>`;
      })
      .join('');
  });

  constructor(public editor: EditorService) {}
}
