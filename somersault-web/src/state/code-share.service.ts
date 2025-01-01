//create CRUD service for code share

import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { UserService } from './user.service';

interface CodeShare {
  content: string;
  modified: number;
  title: string;
}

@Injectable({ providedIn: 'root' })
export class CodeShareService {
  constructor(private _http: HttpClient, private _user: UserService) {}

  load(userId: string, id: string) {
    return this._http.get<CodeShare>(
      `https://next.sannybuilder.com/api/content?id=${id}&author=${userId}`
    );
  }

  create(content: string, title: string = '') {
    const token = this._user.token();
    if (!token) {
      throw new Error('No token found');
    }
    return this._http.post<{ modified: number; id: string }>(
      `https://next.sannybuilder.com/api/content`,
      {
        content,
        title,
      },
      {
        headers: {
          Authorization: `Bearer ${token}`,
        },
      }
    );
  }

  update(id: string, content: string, title: string = '') {
    const token = this._user.token();
    if (!token) {
      throw new Error('No token found');
    }
    return this._http.put<{ modified: number; id: string }>(
      `https://next.sannybuilder.com/api/content`,
      {
        id,
        content,
        title,
      },
      {
        headers: {
          Authorization: `Bearer ${token}`,
        },
      }
    );
  }
}
