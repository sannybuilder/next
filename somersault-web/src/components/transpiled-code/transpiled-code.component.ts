import { Component, computed } from '@angular/core';
import { EditorService } from '../../state/editor.service';
import { NgFor } from '@angular/common';
declare const hljs: any;

@Component({
  selector: 'app-transpiled-code',
  imports: [NgFor],
  templateUrl: './transpiled-code.component.html',
  styleUrl: './transpiled-code.component.scss',
})
export class TranspiledCodeComponent {
  lines = computed(() => {
    try {
      const ir = this._editor.transpiled();

      let highlighted = hljs.highlight(ir, {
        language: 'javascript',
      }).value;
      return highlighted.split('\n');
    } catch (e: any) {
      return [];
    }
  });
  
  constructor(private _editor: EditorService) {}
}
