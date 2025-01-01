import { Component, computed, signal } from '@angular/core';
import { RouterModule } from '@angular/router';
import { TranspiledCodeComponent } from '../transpiled-code/transpiled-code.component';
import { HexCodeComponent } from '../hex-code/hex-code.component';
import { CommonModule } from '@angular/common';
import { TerminalComponent } from '../terminal/terminal.component';
import { EditorService } from '../../state/editor.service';
import { FsService } from '../../state/fs.service';
import { UiService } from '../../state/ui.service';
import { UserService } from '../../state/user.service';
import { DomSanitizer, SafeUrl } from '@angular/platform-browser';
import { persistentSignal } from '../../state/persistent-signal';

@Component({
  selector: 'app-layout',
  imports: [
    CommonModule,
    RouterModule,
    TranspiledCodeComponent,
    HexCodeComponent,
    TerminalComponent,
  ],
  templateUrl: './layout.component.html',
  styleUrl: './layout.component.scss',
})
export class LayoutComponent {
  currentTab = persistentSignal('next.ui.tab', 0);
  user = this._user.user;
  isAuthenticated = this._user.isAuthenticated;

  fileHandle: FileSystemFileHandle | undefined;
  saveStatus = 0;
  copyStatus = 0;

  // sblUrl = computed(() => {
  //   const url = this._ui.sblUrl();
  //   return this.sanitizer.bypassSecurityTrustResourceUrl(url);
  // });

  constructor(
    private _editor: EditorService,
    private _ui: UiService,
    private _fs: FsService,
    private _user: UserService,
    private sanitizer: DomSanitizer
  ) {}

  async saveToFile() {
    if (!this.supportsFs()) {
      return;
    }

    const handle = (this.fileHandle ||= await this._fs.getNewFileHandle());
    this.saveStatus = 1;
    const text = this._editor.transpiled();

    await this._fs.writeFile(handle, text);

    this.saveStatus = 2;

    setTimeout(() => {
      this.saveStatus = 0;
    }, 2000);
  }

  supportsFs() {
    return 'showSaveFilePicker' in window;
  }

  download() {
    const tab = this.currentTab();
    const title = this._ui.title();
    switch (tab) {
      case 0:
        const text = this._editor.transpiled();

        const element = document.createElement('a');
        element.setAttribute(
          'href',
          'data:text/plain;charset=utf-8,' + encodeURIComponent(text)
        );
        element.setAttribute('download', `${title.toLowerCase()}.txt`);
        element.style.display = 'none';
        document.body.appendChild(element);
        element.click();
        document.body.removeChild(element);
        break;

      case 1: {
        const data = this._editor.compiled();
        const chars = data.trim().split(' ');

        const bytes = new Uint8Array(chars.length);
        for (let i = 0; i < chars.length; i += 1) {
          bytes[i] = parseInt(chars[i], 16);
        }

        const blob = new Blob([bytes], { type: 'application/octet-stream' });

        const element = document.createElement('a');
        element.href = URL.createObjectURL(blob);
        element.setAttribute('download', `${title.toLowerCase()}.cs`);
        element.style.display = 'none';
        document.body.appendChild(element);
        element.click();
        document.body.removeChild(element);
        break;
      }
    }
  }

  logout() {
    this._user.logout();
  }

  login() {
    this._user.login();
  }

  copy() {
    const tab = this.currentTab();
    switch (tab) {
      case 0:
        navigator.clipboard.writeText(this._editor.transpiled());
        break;
      case 1:
        navigator.clipboard.writeText(this._editor.compiled());

        break;
    }

    this.copyStatus = 1;

    setTimeout(() => {
      this.copyStatus = 0;
    }, 2000);
  }
}
