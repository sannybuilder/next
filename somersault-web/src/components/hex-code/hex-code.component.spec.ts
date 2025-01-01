import { ComponentFixture, TestBed } from '@angular/core/testing';

import { HexCodeComponent } from './hex-code.component';

describe('HexCodeComponent', () => {
  let component: HexCodeComponent;
  let fixture: ComponentFixture<HexCodeComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      imports: [HexCodeComponent]
    })
    .compileComponents();

    fixture = TestBed.createComponent(HexCodeComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
